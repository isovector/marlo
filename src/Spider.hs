{-# LANGUAGE PartialTypeSignatures           #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE NoMonoLocalBinds                #-}
{-# LANGUAGE NoMonomorphismRestriction       #-}


module Spider where

import           Control.Exception
import           Control.Monad (when, forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))
import           DB
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time (getCurrentTime)
import           Data.Traversable (for)
import           Domains (getDomain)
import           Marlo.Filestore (writeFilestore)
import           Marlo.Robots
import           Marlo.TitleSegs (buildTitleSegs)
import           Network.HttpUtils (determineHttpsAvailability)
import           Network.URI (URI)
import           Prelude hiding (max)
import           Rel8 hiding (filter, bool, index)
import           Rel8.Headers (headersToHeaders)
import           Rel8.TextSearch
import           Signals
import           Types
import           Utils (runRanker, unsafeURI, random, downloadBody, runRankerFS)


nextToExplore :: Query (Discovery Expr)
nextToExplore = limit 1 $ orderBy random $ do
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


markDead :: Connection -> DiscId -> IO ()
markDead conn did =
  doUpdate_ conn $ Update
    { target = discoverySchema
    , from = pure ()
    , set = const $ \d -> d { disc_dead = lit True }
    , updateWhere = const $ \d -> disc_id d ==. lit did
    , returning = pure ()
    }


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
  putStrLn $ "canonical: " <> show (fmap snd mcandl)
  mcandoc <-
    for mcandl $ \(dl, can) -> do
      ed <- getDocByCanonicalUri conn can
      case ed of
        Right d -> do
          putStrLn "already indexed"
          pure $ d_docId d

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

  doUpdate_ conn $ Update
    { target = discoverySchema
    , from = pure ()
    , set = const $ \d ->
        case mcandoc of
          Nothing -> d { disc_dead = lit True }
          Just docid -> d { disc_canonical = lit $ Just docid }
    , updateWhere = const $ \d -> disc_id d ==. lit (disc_id disc)
    , returning = pure ()
    }


reindex :: Connection -> DocId -> Filestore -> IO ()
reindex conn did fs = do
  let uri = fs_uri fs
      run = runRankerFS conn fs

  (dom, directives) <- getDomain conn uri
  let can_index = checkRobotsDirectives directives (CanIndex uri)

  Just !pol <- run isSpiritualPollution
  when (can_index && not pol) $ do
    writeFilestore fs

    Just !ls <- run links
    insertEdges conn did ls

    Just !ts    <- run title
    t <- buildTitleSegs conn did ts

    Just !pc    <- run rankContent
    Just !stats <- run rankStats

    doUpdate_ conn $ Update
      { target = documentSchema
      , from = pure ()
      , set = const $ \d -> d
          { d_domain = lit $ Just dom
          , d_title  = lit t
          , d_search = lit $ Tsvector
              [ (A, ts)
              , (B, pc_headings pc)
              , (C, pc_content pc)
              , (D, pc_comments pc)
              ]
          , d_stats  = lit stats
          }

      , updateWhere = const $ \d -> d_docId d ==. lit did
      , returning = pure ()
      }
    pure ()


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

  doInsert_ conn $ Insert
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


spiderMain :: IO ()
spiderMain = do
  Right conn <- connect
  z <- doInsert conn rootNodes
  print z
  forever $ do
    Right [disc] <- doSelect conn nextToExplore

    let url = disc_uri disc
    putStrLn $ "fetching " <> T.unpack url
    catch
      (do
        discover conn disc
      )
      (\SomeException{} -> do
        putStrLn "failed"
        markDead conn (disc_id disc)
      )

