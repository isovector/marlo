{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Spider where

import           Control.Exception.Base
import           Control.Monad (forever, void, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))
import           DB
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.Foldable (for_, toList)
import           Data.Functor.Contravariant ((>$<))
import           Data.Int (Int32, Int16, Int64)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust, fromMaybe, listToMaybe)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time (getCurrentTime)
import           Data.Traversable (for)
import           Domains (getDomain)
import           Marlo.Filestore (writeFilestore)
import           Marlo.Manager (marloManager)
import           Marlo.Robots
import           Network.HTTP (lookupHeader, HeaderName (HdrSetCookie))
import           Network.HttpUtils (determineHttpsAvailability)
import           Network.URI (parseURI, URI)
import           Prelude hiding (max)
import qualified Rel8 as R8 hiding (filter, bool)
import           Rel8 hiding (filter, bool, index)
import           Rel8.Arrays (arrayInc, arrayZipWithLeast)
import           Rel8.Headers (headersToHeaders)
import           Rel8.Machinery
import           Signals
import           Types
import           Utils (runRanker, unsafeURI, random, downloadBody, titleSegs, runRankerFS)
import Control.DeepSeq (force)


nextToExplore :: Query (Discovery Expr)
nextToExplore = orderBy random $ do
  disc <- each discoverySchema
  where_ $ disc_canonical disc ==. lit Nothing
       &&. disc_dead      disc ==. lit False
  pure disc


getCanonicalUri
    :: Connection
    -> URI
    -> IO (Maybe (Download Maybe ByteString, URI))
getCanonicalUri conn uri = do
  determineHttpsAvailability uri >>= \case
    Nothing -> pure Nothing
    Just uri' -> do
      dl <- liftIO $ downloadBody $ show uri'
      mcanon <- runMaybeT $ do
        uri''  <- MaybeT $ runRanker (Env uri' conn) (decodeUtf8 $ dl_body dl) canonical
        MaybeT $ determineHttpsAvailability uri''
      pure $ pure $ (dl, ) $ fromMaybe uri' mcanon


getDocByCanonicalUri
    :: Connection
    -> URI
    -> IO (Either DocId (Document Identity))
getDocByCanonicalUri conn (T.pack . show -> uri) = do
  Right res <- fmap (fmap listToMaybe) $ doSelect conn $ do
    d <- each documentSchema
    where_ $ d_uri d ==. lit uri
    pure d
  case res of
    Just doc -> pure $ Right doc
    Nothing -> do
      Right [doc] <- doInsert conn $ Insert
        { into = documentSchema
        , rows = do
            did <- nextDocId
            pure $ (lit emptyDoc)
              { d_docId = did
              , d_uri   = lit uri
              }
        , onConflict = DoNothing
        , returning  = Projection d_docId
        }
      pure $ Left doc


discover :: Connection -> Discovery Identity -> IO ()
discover conn disc = do
  mcandl <- getCanonicalUri conn (unsafeURI $ T.unpack $ disc_uri disc)
  mcandoc <-
    for mcandl $ \(dl, can) -> do
      ed <- getDocByCanonicalUri conn can
      case ed of
        Right d -> pure $ d_docId d
        -- It's a new record, so we need to index it
        Left did -> do
          now <- getCurrentTime
          let fs :: Filestore
              fs = Filestore
                    { fs_uri = can
                    , fs_collected = now
                    , fs_headers = fmap headersToHeaders $ dl_headers dl
                    , fs_data = dl_body dl
                    }
          reindex conn did fs
          pure did

  Right _ <- doUpdate conn $ Update
    { target = discoverySchema
    , from = pure ()
    , set = const $ \d ->
        case mcandoc of
          Nothing -> d { disc_dead = lit True }
          Just docid -> d { disc_canonical = lit $ Just docid }
    , updateWhere = const $ \d -> disc_id d ==. lit (disc_id disc)
    , returning = pure ()
    }

  pure ()


reindex :: Connection -> DocId -> Filestore -> IO ()
reindex conn did fs = do
  writeFilestore fs
  Just !ls <- runRankerFS conn fs links
  insertEdges conn did ls


insertEdges :: Connection -> DocId -> [URI] -> IO ()
insertEdges conn did ls = do
  Right discs <- doInsert conn $ Insert
    { into = discoverySchema
    , onConflict = DoNothing
    , returning = Projection disc_id
    , rows = do
        dst <- values $ fmap (lit . T.pack . show) ls
        pure $ (lit emptyDiscovery)
          { disc_uri = dst
          }
    }

  Right _ <- doInsert conn $ Insert
    { into = edgesSchema
    , onConflict = DoNothing
    , returning = pure ()
    , rows = do
        dst <- values $ fmap lit discs
        pure $ Edges
          { e_src = lit did
          , e_dst = dst
          }
    }
  pure ()




-- markExplored :: DocumentState -> Document Identity -> Update ()
-- markExplored ds d = Update
--   { target = documentSchema
--   , from = pure ()
--   , set = \ _ dis -> dis { d_state = lit ds }
--   , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId d)
--   , returning = pure ()
--   }


-- docIdFor :: URI -> Query (Document Expr)
-- docIdFor uri = do
--   dis <- each documentSchema
--   where_ $ d_uri dis ==. lit (T.pack $ show uri)
--   pure dis



-- getDoc :: Connection -> Int32 -> [Maybe Int16] -> URI -> IO (Document Identity)
-- getDoc conn depth dist uri = do
--     Right dids <- doSelect conn $ docIdFor uri
--     case dids of
--       [did] -> pure did
--       [] -> do
--         -- putStrLn $ "discovering " <> show uri
--         Right dids' <- doInsert conn $ discover depth dist uri
--         pure $ head dids'
--       _ -> error $ "invalid database state: " <> show dids


-- discover :: Int32 -> [Maybe Int16] -> URI -> Insert [Document Identity]
-- discover depth dist uri = Insert
--   { into = documentSchema
--   , rows = do
--       docid <- nextDocId
--       pure $ (lit emptyDoc)
--         { d_docId = docid
--         , d_uri = lit $ T.pack $ show uri
--         , d_state = lit Discovered
--         , d_distance = arrayInc $ lit dist
--         , d_depth = lit $ depth + 1
--         }
--   , onConflict = DoUpdate Upsert
--       { index = d_uri
--       , set = \new old -> old
--           { d_depth = leastExpr (d_depth old) (d_depth new)
--           , d_distance = arrayZipWithLeast (d_distance old) (d_distance new)
--           }
--       , updateWhere = \new old -> d_uri new ==. d_uri old
--       }
--   , returning = Projection id
--   }


-- addEdge :: DocId -> Link DocId -> Insert ()
-- addEdge src (Link _ dst) = Insert
--   { into = edgesSchema
--   , rows = do
--       pure $ Edges (lit src) (lit dst)
--   , onConflict = DoNothing
--   , returning = pure ()
--   }



-- buildEdges :: Connection -> Document Identity -> [Link URI] -> IO ()
-- buildEdges conn disc ls = do
--   ldocs' <- (traverse . traverse) (getDoc conn (d_depth disc) (d_distance disc)) ls
--   let ldocs = filter ((/= d_docId disc) . l_uri) $ fmap (fmap d_docId) ldocs'

--   for_ ldocs $ \l -> do
--     Right _ <- doInsert conn $ addEdge (d_docId disc) l
--     pure ()


-- spiderMain :: Maybe Text -> IO ()
-- spiderMain mexclude = do
--   Right conn <- connect
--   z <- doInsert conn rootNodes
--   print z
--   forever $ do
--     Right [disc] <- doSelect conn $
--       limit 1 $ orderBy ((d_depth >$< asc) <> random) $ do
--         d <- each documentSchema
--         where_ $ d_state d ==. lit Discovered
--         for_ mexclude $ \exc ->
--           where_ $ not_ $ like (lit exc) (d_uri d)
--         pure d

--     let url = d_uri disc
--     case parseURI $ T.unpack url of
--       Nothing -> error $ "died on bad URI: " <> show url
--       Just uri -> do
--         case isAcceptableLink uri of
--           True -> do
--             putStrLn $ "fetching " <> T.unpack url
--             catch
--               (do
--                 down <- fmap (sequenceDownload "text/html") $ downloadBody $ T.unpack url
--                 now <- getCurrentTime
--                 writeFilestore $ Filestore
--                   {fs_uri = uri
--                   , fs_collected = now
--                   , fs_headers = fmap headersToHeaders $ Types.d_headers down
--                   , fs_data = d_body down
--                   }
--                 index conn (d_depth disc) (d_distance disc) uri down
--               )
--               (\SomeException{} -> do
--                 putStrLn $ "errored on " <> show (d_docId disc)
--                 void $ doUpdate conn $ markExplored Errored disc
--               )
--           False -> do
--             putStrLn $ "ignoring " <> T.unpack url
--             void $ doUpdate conn $ markExplored Pruned disc


-- index :: Connection -> Int32 -> [Maybe Int16] -> URI -> Download Identity ByteString -> IO ()
-- index conn depth dist uri down = do
--   (dom, directives) <- getDomain conn uri
--   disc <- getDoc conn depth dist uri

--   let can_index = checkRobotsDirectives directives (CanIndex uri)

--   case ( d_mime down == "text/html"
--       && isAcceptableLink uri
--       && can_index
--        ) of

--     True -> do
--       let env = Env uri marloManager conn
--       Just (ls, t) <- runRanker env (decodeUtf8 $ d_body down) $ (,) <$> links <*> title

--       let raw =
--             PageRawData
--               { prd_data = d_body down
--               , prd_headers = fmap headersToHeaders $ Types.d_headers down
--               }
--       Right _ <- doUpdate conn $ Update
--         { target = documentSchema
--         , from = pure ()
--         , set = \ _ dis -> dis
--             { d_state = lit Explored
--             , d_title = lit t
--             , d_raw = lit raw
--             , d_domain = lit $ Just dom
--             }
--         , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId disc)
--         , returning = pure ()
--         }
--       indexCore conn env $ disc
--         { d_state = Explored
--         , d_title = t
--         , d_raw = raw
--         , d_domain = Just dom
--         }
--       void $ buildEdges conn disc ls

--     False -> do
--       Right _
--         <- doUpdate conn
--           $ flip markExplored disc
--           $ bool DisallowedByRobots Pruned can_index
--       pure ()


-- indexFromDB :: Connection -> Document Identity -> IO ()
-- indexFromDB conn disc = do
--   let uri = unsafeURI $ T.unpack $ d_uri disc
--   case (isAcceptableLink uri) of
--     True -> indexCore conn (Env uri marloManager conn) disc
--     False -> do
--       void $ doUpdate conn $ markExplored Pruned disc


-- indexCore :: Connection -> Env -> Document Identity -> IO ()
-- indexCore conn env disc = do
--   let uri = unsafeURI $ T.unpack $ d_uri disc
--   (dom_id, _) <- getDomain conn uri
--   Just (titl, pc, is_pollution, stats', canuri) <-
--     runRanker env (decodeUtf8 $ prd_data $ d_raw disc) $
--       (,,,,)
--         <$> title
--         <*> rankContent
--         <*> isSpiritualPollution
--         <*> rankStats
--         <*> canonical
--   let stats = stats'
--         { ps_cookies
--             = isJust
--             $ lookupHeader HdrSetCookie
--             $ prd_headers
--             $ d_raw disc
--         }

--   for_ canuri $ \can ->
--     putStrLn $ "canonical for " <> show uri <> " is " <> show can

--   unless is_pollution $
--     buildTitleSegs conn (d_docId disc) titl

--   -- TODO(sandy): bug??? headers aren't being set
--   Right () <-  doUpdate conn $ Update
--     { target = documentSchema
--     , from = pure ()
--     , set = \ _ dis -> dis
--         { d_uri    = maybe (d_uri dis) (lit . T.pack . show) canuri
--         , d_domain = lit $ Just dom_id
--         , d_page   = lit pc
--         , d_state  = lit $ bool Explored Unacceptable is_pollution
--         , d_stats  = lit stats
--         }
--     , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId disc)
--     , returning = pure ()
--     }
--   pure ()


-- debugRankerOnline :: Text -> Ranker a -> IO (Text, Maybe a)
-- debugRankerOnline (T.unpack -> uri) r = do
--   Right conn <- connect
--   z <- downloadBody uri
--   let env = Env (unsafeURI uri) marloManager conn
--   fmap (T.pack uri, ) $
--     runRanker env (decodeUtf8 $ d_body z) r

-- debugRankerInDb :: Text -> Ranker a -> IO (Text, Maybe a)
-- debugRankerInDb uri r = do
--   Right conn <- connect
--   Right [d] <-
--     doSelect conn $ limit 1 $ do
--       d <- each documentSchema
--       where_ $ (like (lit $ "%" <> uri <> "%") $ d_uri d) &&. d_state d ==. lit Explored
--       pure d
--   let env = Env (unsafeURI $ T.unpack $ d_uri d) marloManager conn
--   fmap (d_uri d, ) $
--     runRanker env (decodeUtf8 $ prd_data $ d_raw d) r

