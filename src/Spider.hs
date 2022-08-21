{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Spider where

import           Control.Exception.Base
import           Control.Monad (forever, void, unless)
import           DB
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.Foldable (for_, toList)
import           Data.Functor.Contravariant ((>$<))
import           Data.Int (Int32, Int16, Int64)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Traversable (for)
import           Domains (getDomain)
import           Marlo.Manager (marloManager)
import           Marlo.Robots
import           Network.HTTP (lookupHeader, HeaderName (HdrSetCookie))
import           Network.URI (parseURI, URI)
import           Prelude hiding (max)
import qualified Rel8 as R8 hiding (filter, bool)
import           Rel8 hiding (filter, bool, index)
import           Rel8.Arrays (arrayInc, arrayZipWithLeast)
import           Rel8.Headers (headersToHeaders)
import           Rel8.Machinery
import           Signals
import qualified Types
import           Types hiding (d_headers)
import           Utils (runRanker, unsafeURI, random, downloadBody, titleSegs)


markExplored :: DocumentState -> Document Identity -> Update ()
markExplored ds d = Update
  { target = documentSchema
  , from = pure ()
  , set = \ _ dis -> dis { d_state = lit ds }
  , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId d)
  , returning = pure ()
  }


docIdFor :: URI -> Query (Document Expr)
docIdFor uri = do
  dis <- each documentSchema
  where_ $ d_uri dis ==. lit (T.pack $ show uri)
  pure dis



getDoc :: Connection -> Int32 -> [Maybe Int16] -> URI -> IO (Document Identity)
getDoc conn depth dist uri = do
    Right dids <- doSelect conn $ docIdFor uri
    case dids of
      [did] -> pure did
      [] -> do
        -- putStrLn $ "discovering " <> show uri
        Right dids' <- doInsert conn $ discover depth dist uri
        pure $ head dids'
      _ -> error $ "invalid database state: " <> show dids


discover :: Int32 -> [Maybe Int16] -> URI -> Insert [Document Identity]
discover depth dist uri = Insert
  { into = documentSchema
  , rows = do
      docid <- nextDocId
      pure $ (lit emptyDoc)
        { d_docId = docid
        , d_uri = lit $ T.pack $ show uri
        , d_state = lit Discovered
        , d_distance = arrayInc $ lit dist
        , d_depth = lit $ depth + 1
        }
  , onConflict = DoUpdate Upsert
      { index = d_uri
      , set = \new old -> old
          { d_depth = leastExpr (d_depth old) (d_depth new)
          , d_distance = arrayZipWithLeast (d_distance old) (d_distance new)
          }
      , updateWhere = \new old -> d_uri new ==. d_uri old
      }
  , returning = Projection id
  }


addEdge :: DocId -> Link DocId -> Insert [EdgeId]
addEdge src (Link anchor dst) = Insert
  { into = edgesSchema
  , rows = do
      eid <- nextEdgeId
      pure $ Edges eid (lit src) (lit dst) $ lit anchor
  , onConflict = DoNothing
  , returning = Projection e_edgeId
  }


buildTitleSegs :: Connection -> DocId -> Text -> IO ()
buildTitleSegs conn doc t = do
  let segs = titleSegs t
  segids <- getOrInsert conn TitleSeg ts_seg segs
  Right _ <- doInsert conn $ Insert
    { into = titleEdgeSchema
    , rows = do
        teid <- nextTitleEdgeId
        seg <- values $ fmap lit $ toList segids
        pure $ TitleEdge
          { te_id = teid
          , te_doc = lit doc
          , te_seg = seg
          }
    , onConflict = DoNothing
    , returning = pure ()
    }
  pure ()


commonTitleSegs :: Query (Expr TitleSegId, Expr Int64)
commonTitleSegs = do
  orderBy ((snd >$< asc) <> (fst >$< asc)) $ aggregate $ do
    te <- each titleEdgeSchema
    pure (groupBy $ te_seg te, countStar)


getBestTitle :: DocId -> Query (Expr Text)
getBestTitle did = do
  segid <- limit 1 $ fmap fst $ orderBy (snd >$< asc) $ do
    te <- each titleEdgeSchema
    where_ $ te_doc te ==. lit did
    res <- commonTitleSegs
    where_ $ te_seg te ==. fst res
    pure res
  seg <- each titleSegSchema
  where_ $ ts_id seg ==. segid
  pure $ ts_seg seg


setBestTitle :: DocId -> Update [Text]
setBestTitle did = Update
  { target = documentSchema
  , from = getBestTitle did
  , set = \ ex doc -> doc { d_title = ex }
  , updateWhere = \ _ doc -> d_docId doc ==. lit did
  , returning = Projection d_title
  }


getOrInsert
    :: (HasUniqueId f key, Ord a, _)
    => Connection
    -> (Expr key -> Expr a -> f Expr)
    -> (f Expr -> Expr a)
    -> [a]
    -> IO (Map a key)
getOrInsert conn build what as = do
  let all_vals = S.fromList as
  Right vals <- fmap (fmap M.fromList) $ doSelect conn $ do
    r <- each allRows
    where_ $ in_ (what r) $ fmap lit as
    pure (what r, uniqueId r)
  let missing_vals = all_vals S.\\ M.keysSet vals
  Right more_vals <- fmap (fmap M.fromList) $ doInsert conn $ Insert
    { into = allRows
    , rows = do
        rid <- nextUniqueId
        a <- values $ fmap lit $ S.toList missing_vals
        pure $ build rid a
    , onConflict = DoNothing
    , returning = Projection $ \r -> (what r, uniqueId r)
    }
  pure $ vals <> more_vals


buildEdges :: Connection -> Document Identity -> [Link URI] -> IO [EdgeId]
buildEdges conn disc ls = do
  ldocs' <- (traverse . traverse) (getDoc conn (d_depth disc) (d_distance disc)) ls
  let ldocs = filter ((/= d_docId disc) . l_uri) $ fmap (fmap d_docId) ldocs'

  for ldocs $ \l -> do
    Right [eid] <- doInsert conn $ addEdge (d_docId disc) l
    pure eid


spiderMain :: Maybe Text -> IO ()
spiderMain mexclude = do
  Right conn <- connect
  z <- doInsert conn rootNodes
  print z
  forever $ do
    Right [disc] <- doSelect conn $
      limit 1 $ orderBy ((d_depth >$< asc) <> random) $ do
        d <- each documentSchema
        where_ $ d_state d ==. lit Discovered
        for_ mexclude $ \exc ->
          where_ $ not_ $ like (lit exc) (d_uri d)
        pure d

    let url = d_uri disc
    case parseURI $ T.unpack url of
      Nothing -> error $ "died on bad URI: " <> show url
      Just uri -> do
        case isAcceptableLink uri of
          True -> do
            putStrLn $ "fetching " <> T.unpack url
            catch
              (do
                down <- fmap (sequenceDownload "text/html") $ downloadBody $ T.unpack url
                index conn (d_depth disc) (d_distance disc) uri down
              )
              (\SomeException{} -> do
                putStrLn $ "errored on " <> show (d_docId disc)
                void $ doUpdate conn $ markExplored Errored disc
              )
          False -> do
            putStrLn $ "ignoring " <> T.unpack url
            void $ doUpdate conn $ markExplored Pruned disc


index :: Connection -> Int32 -> [Maybe Int16] -> URI -> Download Identity ByteString -> IO ()
index conn depth dist uri down = do
  (dom, directives) <- getDomain conn uri
  disc <- getDoc conn depth dist uri

  let can_index = checkRobotsDirectives directives (CanIndex uri)

  case ( d_mime down == "text/html"
      && isAcceptableLink uri
      && can_index
       ) of

    True -> do
      let env = Env uri marloManager conn
      Just (ls, t) <- runRanker env (decodeUtf8 $ d_body down) $ (,) <$> links <*> title

      let raw =
            PageRawData
              { prd_data = d_body down
              , prd_headers = fmap headersToHeaders $ Types.d_headers down
              }
      Right _ <- doUpdate conn $ Update
        { target = documentSchema
        , from = pure ()
        , set = \ _ dis -> dis
            { d_state = lit Explored
            , d_title = lit t
            , d_raw = lit raw
            , d_domain = lit $ Just dom
            }
        , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId disc)
        , returning = pure ()
        }
      indexCore conn env $ disc
        { d_state = Explored
        , d_title = t
        , d_raw = raw
        , d_domain = Just dom
        }
      void $ buildEdges conn disc ls

    False -> do
      Right _
        <- doUpdate conn
          $ flip markExplored disc
          $ bool DisallowedByRobots Pruned can_index
      pure ()


indexFromDB :: Connection -> Document Identity -> IO ()
indexFromDB conn disc = do
  let uri = unsafeURI $ T.unpack $ d_uri disc
  case (isAcceptableLink uri) of
    True -> indexCore conn (Env uri marloManager conn) disc
    False -> do
      void $ doUpdate conn $ markExplored Pruned disc


indexCore :: Connection -> Env -> Document Identity -> IO ()
indexCore conn env disc = do
  let uri = unsafeURI $ T.unpack $ d_uri disc
  (dom_id, _) <- getDomain conn uri
  Just (titl, pc, is_pollution, stats') <-
    runRanker env (decodeUtf8 $ prd_data $ d_raw disc) $
      (,,,)
        <$> title
        <*> rankContent
        <*> isSpiritualPollution
        <*> rankStats
  let stats = stats'
        { ps_cookies
            = isJust
            $ lookupHeader HdrSetCookie
            $ prd_headers
            $ d_raw disc
        }

  unless is_pollution $
    buildTitleSegs conn (d_docId disc) titl

  -- TODO(sandy): bug??? headers aren't being set
  Right () <-  doUpdate conn $ Update
    { target = documentSchema
    , from = pure ()
    , set = \ _ dis -> dis
        { d_domain = lit $ Just dom_id
        , d_page = lit pc
        , d_state    = lit $ bool Explored Unacceptable is_pollution
        , d_stats    = lit stats
        }
    , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId disc)
    , returning = pure ()
    }
  pure ()


debugRankerOnline :: Text -> Ranker a -> IO (Text, Maybe a)
debugRankerOnline (T.unpack -> uri) r = do
  Right conn <- connect
  z <- downloadBody uri
  let env = Env (unsafeURI uri) marloManager conn
  fmap (T.pack uri, ) $
    runRanker env (decodeUtf8 $ d_body z) r

debugRankerInDb :: Text -> Ranker a -> IO (Text, Maybe a)
debugRankerInDb uri r = do
  Right conn <- connect
  Right [d] <-
    doSelect conn $ limit 1 $ do
      d <- each documentSchema
      where_ $ (like (lit $ "%" <> uri <> "%") $ d_uri d) &&. d_state d ==. lit Explored
      pure d
  let env = Env (unsafeURI $ T.unpack $ d_uri d) marloManager conn
  fmap (d_uri d, ) $
    runRanker env (decodeUtf8 $ prd_data $ d_raw d) r

