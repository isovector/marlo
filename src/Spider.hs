{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wall              #-}

module Spider where

import Data.Bool (bool)
import           Control.Exception.Base
import           Control.Monad (forever, when, void)
import           DB
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor.Contravariant ((>$<))
import           Data.Functor.Identity (Identity)
import           Data.Int (Int32)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Traversable (for)
import           Hasql.Connection (acquire, Connection)
import           Hasql.Session (run, statement)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import           Network.HTTP.Types (hContentType)
import           Network.URI (parseURI, URI)
import           Rel8 hiding (filter, bool, index)
import           Signals
import           Types
import           Utils (runRanker, unsafeURI, random)
import qualified Rel8 as R8 hiding (filter, bool)
import qualified Data.ByteString.Lazy as BSL

--

nextDiscovered :: Query (Discovery Expr)
nextDiscovered = limit 1 $ orderBy ((d_depth >$< asc) <> random) $ do
  d <- each discoverySchema
  where_ $ d_state d ==. lit Discovered
  pure d


markExplored :: DiscoveryState -> Discovery Identity -> Update ()
markExplored ds d = Update
  { target = discoverySchema
  , from = pure ()
  , set = \ _ dis -> dis { d_state = lit ds }
  , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId d)
  , returning = pure ()
  }


docIdFor :: URI -> Query (Discovery Expr)
docIdFor uri = do
  dis <- each discoverySchema
  where_ $ d_uri dis ==. lit (T.pack $ show uri)
  pure dis



getDocId :: Connection -> Int32 -> URI -> IO (Discovery Identity)
getDocId conn depth uri = do
    Right dids <- flip run conn $ statement () $ select $ docIdFor uri
    case dids of
      [did] -> pure did
      [] -> do
        -- putStrLn $ "discovering " <> show uri
        Right dids' <- flip run conn $ statement () $ insert $ discover depth uri
        pure $ head dids'
      _ -> error $ "invalid database state: " <> show dids


discover :: Int32 -> URI -> Insert [Discovery Identity]
discover depth uri = Insert
  { into = discoverySchema
  , rows = do
      docid <- nextDocId
      pure $
        Discovery
          docid
          (lit $ T.pack $ show uri)
          (lit Discovered)
          (lit $ depth + 1)
          (lit "")
          (lit 0)
          (lit "")
          (lit "")
          (lit "")
          (lit "")
  , onConflict = DoUpdate $ Upsert
                  { index = d_docId
                  , set = \new old -> old { d_depth = leastExpr (d_depth old) (d_depth new) }
                  , updateWhere = \new old -> d_docId new ==. d_docId old
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


buildEdges :: Connection -> Discovery Identity -> [Link URI] -> IO [EdgeId]
buildEdges conn disc ls = do
  ldocs' <- (traverse . traverse) (getDocId conn $ d_depth disc) ls
  let ldocs = filter ((/= d_docId disc) . l_uri) $ fmap (fmap d_docId) ldocs'

  for ldocs $ \l -> do
    -- putStrLn $ "edge ->" <> show (did, l_uri l)
    Right [eid] <- flip run conn $ statement () $ insert $ addEdge (d_docId disc) l
    pure eid



-- things still to do:
-- - only index in en
-- - cache/zip results
-- - ranking
-- - do we get a 404 if we hit a random url?
-- -

spiderMain :: IO ()
spiderMain = do
  Right conn <- acquire connectionSettings
  z <- flip run conn $ statement () $ insert rootNodes
  print z
  forever $ do
    Right [disc] <- flip run conn $ statement () $ select nextDiscovered
    let url = d_uri disc
    case parseURI $ T.unpack url of
      Nothing -> error $ "died on bad URI: " <> show url
      Just uri -> do
        case isAcceptableLink uri of
          True -> do
            putStrLn $ "fetching " <> T.unpack url
            catch
              (do
                (Just mime, raw_body) <- downloadBody $ T.unpack url
                let body = decodeUtf8 raw_body
                index conn (d_depth disc) uri mime raw_body body
              )
              (\SomeException{} -> do
                putStrLn $ "errored on " <> show (d_docId disc)
                void $ flip run conn $ statement () $ update $ markExplored Errored disc
              )
          False -> do
            putStrLn $ "ignoring " <> T.unpack url
            void $ flip run conn $ statement () $ update $ markExplored Pruned disc

continue :: Connection -> Int32 -> URI -> ByteString -> ByteString -> Text -> IO ()
continue conn depth uri mime raw_body body = do
  disc <- getDocId conn depth uri
  mgr <- HTTP.getGlobalManager
  when (mime == "text/html" && isAcceptableLink uri) $ do
    Just ls <-  runRanker (Env uri mgr conn) body links
    void $ buildEdges conn disc ls
  -- putStrLn $ "explored " <> show did
  void $ flip run conn $ statement () $ update $ Update
    { target = discoverySchema
    , from = pure ()
    , set = \ _ dis -> dis { d_state = lit Explored
                           , d_data  = lit raw_body
                           }
    , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId disc)
    , returning = pure ()
    }



index :: Connection -> Int32 -> URI -> ByteString -> ByteString -> Text -> IO ()
index conn depth uri mime raw_body body = do
  disc <- getDocId conn depth uri
  mgr <- HTTP.getGlobalManager
  case (mime == "text/html" && isAcceptableLink uri) of
    True -> do
      let env = Env uri mgr conn
      Just (ls, t) <- runRanker env  body $ (,) <$> links <*> title
      void $ buildEdges conn disc ls
      Right _ <- flip run conn $ statement () $ update $ Update
        { target = discoverySchema
        , from = pure ()
        , set = \ _ dis -> dis { d_state = lit Explored
                               , d_title = lit t
                               , d_data  = lit raw_body
                              }
        , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId disc)
        , returning = pure ()
        }
      indexCore conn env $ disc
        { d_state = Explored
        , d_title = t
        , d_data = raw_body
        }
    False -> do
      void $ flip run conn $ statement () $ update $ markExplored Pruned disc

indexFromDB :: Connection -> Discovery Identity -> IO ()
indexFromDB conn disc = do
  let uri = unsafeURI $ T.unpack $ d_uri disc
  mgr <- HTTP.getGlobalManager
  case (isAcceptableLink uri) of
    True -> indexCore conn (Env uri mgr conn) disc
    False -> do
      void $ flip run conn $ statement () $ update $ markExplored Pruned disc

indexCore :: Connection -> Env -> Discovery Identity -> IO ()
indexCore conn env disc = do
  Just (m, h, c, has_ads) <-
    runRanker env (decodeUtf8 $ d_data disc) $
      (,,,) <$> mainContent <*> headingsContent <*> commentsContent <*> hasGoogleAds
  Right () <-  flip run conn $ statement () $ update $ Update
    { target = discoverySchema
    , from = pure ()
    , set = \ _ dis -> dis { d_content  = lit m
                           , d_headings = lit h
                           , d_comments = lit c
                           , d_state    = lit $ bool Explored Unacceptable has_ads
                           }
    , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId disc)
    , returning = pure ()
    }
  pure ()


mimeToContentType :: ByteString -> ByteString
mimeToContentType = BS.takeWhile (/= ';')

downloadBody :: String -> IO (Maybe ByteString, ByteString)
downloadBody url = do
  manager <- HTTP.getGlobalManager
  resp <- flip HTTP.httpLbs manager =<< HTTP.parseRequest url
  let mime = fmap mimeToContentType
            $ lookup hContentType
            $ HTTP.responseHeaders resp
  pure $ (mime, ) $ BSL.toStrict $ HTTP.responseBody resp


debugRankerInDb :: Text -> Ranker a -> IO (Text, Maybe a)
debugRankerInDb uri r = do
  Right conn <- acquire connectionSettings
  mgr <- HTTP.getGlobalManager
  Right [d] <-
    flip run conn $ statement () $ select $ limit 1 $ do
      d <- each discoverySchema
      where_ $ (like (lit $ "%" <> uri <> "%") $ d_uri d) &&. d_state d ==. lit Explored
      pure d
  fmap (d_uri d,) $ runRanker (Env (unsafeURI $ T.unpack $ d_uri d) mgr conn) (decodeUtf8 $ d_data d) r


