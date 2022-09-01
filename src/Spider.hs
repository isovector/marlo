{-# LANGUAGE PartialTypeSignatures           #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports          #-}

{-# LANGUAGE NoMonoLocalBinds                #-}
{-# LANGUAGE NoMonomorphismRestriction       #-}


module Spider where

import           Control.DeepSeq (force)
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
import Marlo.TitleSegs (buildTitleSegs)


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
          , d_page   = lit pc
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

