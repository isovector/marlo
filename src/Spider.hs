{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spider where

import Types
import DB
import Signals
import Hasql.Connection (acquire, Connection)
import Control.Monad (forever)
import Hasql.Session (run, statement)
import Rel8
import Network.URI (parseURI, URI)
import qualified Data.Text as T
import Text.HTML.Scalpel (scrapeURL)
import Control.Monad.Reader (runReaderT)
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Client as HTTP
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Utils (runRanker)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity)
import Data.Traversable (for)

main :: IO ()
main = spiderMain

--

nextDiscovered :: Query (Discovery Expr)
nextDiscovered = limit 1 $ do
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

docIdFor :: URI -> Query (Expr DocId)
docIdFor uri = do
  dis <- each discoverySchema
  where_ $ d_uri dis ==. lit (T.pack $ show uri)
  pure $ d_docId dis


getDocId :: Connection -> URI -> IO DocId
getDocId conn uri = do
    Right dids <- flip run conn $ statement () $ select $ docIdFor uri
    case dids of
      [did] -> pure did
      [] -> do
        putStrLn $ "discovering " <> show uri
        Right dids <- flip run conn $ statement () $ insert $ discover uri
        putStrLn $ show dids
        pure $ head dids
      _ -> error $ "invalid database state: " <> show dids


discover :: URI -> Insert [DocId]
discover uri = Insert
  { into = discoverySchema
  , rows = do
      docid <- nextDocId
      pure $ Discovery docid (lit $ T.pack $ show uri) $ lit Discovered
  , onConflict = DoNothing
  , returning = Projection d_docId
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


buildEdges :: Connection -> DocId -> [Link URI] -> IO [EdgeId]
buildEdges conn did ls = do
  ldocs <- (traverse . traverse) (getDocId conn) ls
  for ldocs $ \l -> do
    putStrLn $ "edge ->" <> show (did, l_uri l)
    Right [eid] <- flip run conn $ statement () $ insert $ addEdge did l
    pure eid



spiderMain :: IO ()
spiderMain = do
  Right conn <- acquire connectionSettings
  forever $ do
    Right [disc] <- flip run conn $ statement () $ select nextDiscovered
    let url = d_uri disc
    case parseURI $ T.unpack url of
      Nothing -> error $ "died on bad URI: " <> show url
      Just uri -> do
        putStrLn $ "fetching " <> T.unpack url
        body <- downloadBody $ T.unpack url
        let Just ls = runRanker uri body links
        buildEdges conn (d_docId disc) ls
        putStrLn $ "explored " <> show (d_docId disc)
        flip run conn $ statement () $ update $ markExplored Explored disc

downloadBody :: String -> IO T.Text
downloadBody url = do
    manager <- maybe HTTP.getGlobalManager pure Nothing
    response <- flip HTTP.httpLbs manager =<< HTTP.parseRequest url
    pure $ toStrict $ decodeUtf8 $ HTTP.responseBody response



