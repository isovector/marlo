{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wall              #-}

module Spider where

import           Control.Applicative (liftA2, liftA3)
import           Control.Exception.Base
import           Control.Monad (forever, when, void)
import           DB
import           Data.Bifunctor (first, second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (for_)
import           Data.Functor ((<&>))
import           Data.Functor.Contravariant ((>$<), (>$))
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int32)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Traversable (for)
import           Hasql.Connection (acquire, Connection)
import           Hasql.Session (run, statement)
import           Keywords (posWords)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import           Network.HTTP.Types (hContentType)
import           Network.URI (parseURI, URI)
import           Ranking (rank)
import           Rel8 hiding (index)
import           Signals
import           Types
import           Utils (runRanker)
import qualified Rel8 as R8
import qualified Data.ByteString.Lazy as BSL

-- main :: IO ()
main = searchMain

--

nextDiscovered :: Query (Discovery Expr)
nextDiscovered = limit 1 $ orderBy ((d_depth >$< asc) <> (nullaryFunction @Double "RANDOM" >$ asc)) $ do
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


getWordIds :: [Keyword] -> Query (Words Expr)
getWordIds kws = do
  w <- each wordsSchema
  where_ $ in_ (w_word w) $ fmap (lit . getKeyword) kws
  pure w

createWordIds :: [Keyword] -> Insert [Words Identity]
createWordIds kws = Insert
  { into = wordsSchema
  , rows = do
      wid <- nextWordId
      kw <- values $ fmap (lit . getKeyword) kws
      pure $ Words
        { w_wordId = wid
        , w_word = kw
        }
  , onConflict = DoNothing
  , returning = Projection id
  }


insertKeywords :: DocId -> [(Int, WordId)] -> Insert ()
insertKeywords did kws = Insert
  { into = indexSchema
  , rows = do
      iid <- nextIndexId
      (pos, wid) <- values $ fmap (lit . first (fromIntegral @_ @Int16)) kws
      pure $ Index
        { i_id = iid
        , i_docId  = lit did
        , i_wordId = wid
        , i_position = pos
        }
  , onConflict = DoNothing
  , returning = pure ()
  }


indexWords :: Connection -> DocId -> [(Int, Keyword)] -> IO ()
indexWords conn did pos = do
  let kws = nubOrd $ fmap snd pos
  void $ flip run conn $ statement () $ insert $ createWordIds kws
  Right ws <- flip run conn $ statement () $ select $ getWordIds kws
  let word_map = M.fromList $ ws <&> \(Words a b) -> (Keyword b, a)
      pos' = fmap (second (word_map M.!)) pos
  Right res <- flip run conn $ statement () $ insert $ insertKeywords did pos'
  pure res


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
  ldocs <- (traverse . traverse) (getDocId conn $ d_depth disc) ls
  for ldocs $ \l -> do
    -- putStrLn $ "edge ->" <> show (did, l_uri l)
    Right [eid] <- flip run conn $ statement () $ insert $ addEdge (d_docId disc) $ fmap d_docId l
    pure eid


getDocs :: [WordId] -> Query (Discovery Expr)
getDocs [] = do
  where_ $ true ==. false
  pure $ lit $ Discovery (DocId 0) "" Discovered 0 "" 0
getDocs wids = distinct $ do
  d <- distinct $ foldr1 intersect $ do
    wid <- wids
    pure $ do
      w <- each indexSchema
      where_ $ i_wordId w ==. lit wid
      pure $ i_docId w
  doc <- each discoverySchema
  where_ $ d_docId doc ==. d
  pure doc


search :: Connection -> [Keyword] -> IO [Text]
search conn kws = do
  Right wids <- flip run conn $ statement () $ select $ getWordIds kws
  let not_in_corpus = S.fromList kws S.\\ S.fromList (fmap (Keyword . w_word) wids)
  print not_in_corpus
  -- putStrLn $ showQuery $ getDocs $ fmap w_wordId wids
  Right docs <- flip run conn $ statement () $ select $ getDocs $ fmap w_wordId wids
  pure $ fmap d_uri docs



searchMain :: IO [Text]
searchMain = do
  Right conn <- acquire connectionSettings
  search conn ["date", "duration", "calculator"]


-- things still to do:
-- - only index in en
-- - cache/zip results
-- - ranking
-- - do we get a 404 if we hit a random url?
-- -

spiderMain :: IO ()
spiderMain = do
  Right conn <- acquire connectionSettings
  void $ flip run conn $ statement () $ insert rootNodes
  forever $ do
    Right [disc] <- flip run conn $ statement () $ select nextDiscovered
    let url = d_uri disc
    case parseURI $ T.unpack url of
      Nothing -> error $ "died on bad URI: " <> show url
      Just uri -> do
        putStrLn $ "fetching " <> T.unpack url
        catch
          (do
            (Just mime, raw_body) <- downloadBody $ T.unpack url
            let body = decodeUtf8 raw_body
            continue conn (d_depth disc) uri mime raw_body body
          )
          (\SomeException{} -> do
            putStrLn $ "errored on " <> show (d_docId disc)
            void $ flip run conn $ statement () $ update $ markExplored Errored disc
          )

continue :: Connection -> Int32 -> URI -> ByteString -> ByteString -> Text -> IO ()
continue conn depth uri mime raw_body body = do
  disc <- getDocId conn depth uri
  when (mime == "text/html" && isAcceptableLink uri) $ do
    let Just ls = runRanker uri body links
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



index :: Connection -> Int32 -> URI -> ByteString -> Text -> IO ()
index conn depth uri mime body = do
  disc <- getDocId conn depth uri
  let did = d_docId disc
  when (mime == "text/html" && isAcceptableLink uri) $ do
    let Just (ls, ws, stuff) = runRanker uri body $ liftA3 (,,) links posWords rankStuff
    void $ buildEdges conn disc ls
    indexWords conn did ws
    when (rank stuff > 0) $
       putStrLn $ show uri <> "   : " <> show (rank stuff)
  -- putStrLn $ "explored " <> show did
  void $ flip run conn $ statement () $ update $ markExplored Explored disc


mimeToContentType :: ByteString -> ByteString
mimeToContentType = BS.takeWhile (/= ';')

downloadBody :: String -> IO (Maybe ByteString, ByteString)
downloadBody url = do
    manager <- maybe HTTP.getGlobalManager pure Nothing
    resp <- flip HTTP.httpLbs manager =<< HTTP.parseRequest url
    let mime = fmap mimeToContentType
             $ lookup hContentType
             $ HTTP.responseHeaders resp
    pure $ (mime, ) $ BSL.toStrict $ HTTP.responseBody resp

