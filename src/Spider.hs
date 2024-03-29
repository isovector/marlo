{-# LANGUAGE PartialTypeSignatures           #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE NoMonoLocalBinds                #-}
{-# LANGUAGE NoMonomorphismRestriction       #-}


module Spider where

import           Assets (getAssetSizes)
import           Control.Concurrent.Async (async, wait)
import           Control.Exception
import           Control.Monad (forever, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))
import           DB
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.Functor.Contravariant ((>$<))
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time (getCurrentTime)
import           Data.Traversable (for)
import           Domains (getDomain)
import           GHC.Stack (HasCallStack)
import           Marlo.Filestore (writeFilestore)
import           Marlo.Robots
import           Marlo.TitleSegs (buildTitleSegs)
import           Marlo.URIs (normalizeURI)
import           Network.HttpUtils (determineHttpsAvailability)
import           Network.URI (URI, relativeTo, parseURI)
import           Prelude hiding (max)
import           Rel8 hiding (evaluate, sum, filter, bool, index, optional)
import           Rel8.Headers (headersToHeaders)
import           Rel8.StateMask
import           Rel8.TextSearch
import           Signals.AcceptableURI (isAcceptableLink, forbidPaths, forbidSites)
import           Signals.Content
import           Types
import           Utils (unsafeURI, random, downloadBody, tryIO, parsePermissiveTree, runScraper)


nextToExplore :: Query (Discovery Expr)
nextToExplore = limit 1 $ orderBy ((disc_depth >$< asc) <> random) $ do
  disc <- each discoverySchema
  where_ $ disc_canonical disc ==. lit Nothing
       &&. disc_dead      disc ==. lit False
  pure disc


getCanonicalUri
    :: HasCallStack
    => URI
    -> IO (Maybe (Download Maybe ByteString, URI))
getCanonicalUri uri = do
  dl <- liftIO $ downloadBody $ show uri
  let !tree = parsePermissiveTree $ decodeUtf8 $ dl_body dl
      runL
        = runScraper
        $ fromMaybe (error "failed to parse the html document!") tree
  mcanon <- runMaybeT $ do
    uri'' <- hoistMaybe $ runL canonical
    MaybeT $ determineHttpsAvailability uri''
  fmap (fmap (dl,)) $ case mcanon of
    Nothing   -> determineHttpsAvailability uri
    Just uri' -> pure $ Just uri'


hoistMaybe :: Maybe a -> MaybeT IO a
hoistMaybe = MaybeT . pure


markDead :: Connection -> DiscId -> IO ()
markDead conn did =
  doUpdate_ conn $ Update
    { target = discoverySchema
    , from = pure ()
    , set = const $ \d -> d { disc_dead = lit True }
    , updateWhere = const $ \d -> disc_id d ==. lit did
    , returning = pure ()
    }


incomingDepth :: DocId -> Query (Expr Int32)
incomingDepth did = aggregate $ do
  disc <- each discoverySchema
  where_ $ disc_canonical disc ==. lit (Just did)
  pure $ Rel8.min $ disc_depth disc

incomingDistance :: DocId -> Query (Expr (Distance Int16))
incomingDistance did = do
  disc <- each discoverySchema
  where_ $ disc_canonical disc ==. lit (Just did)
  pure $ disc_distance disc


getDocByCanonicalUri
    :: HasCallStack
    => Connection
    -> URI
    -> IO (Either DocId (Document Identity))
getDocByCanonicalUri conn (T.pack . show -> uri) = do
  z <- fmap (fmap listToMaybe) $ doSelect conn $ do
    d <- each documentSchema
    where_ $ d_uri d ==. lit uri
    pure d
  let Right res = z
  case res of
    Just doc -> pure $ Right doc
    Nothing -> do
      Right [doc] <- doInsert conn $ Insert
        { into = documentSchema'
        , rows = do
            did <- nextDocId
            pure $ Document'
              { d_table = (lit emptyDoc)
                { d_docId = did
                , d_uri   = lit uri
                }
              , d_doc_text = lit ""
              , d_search = lit $ Tsvector []
              }
        , onConflict = DoNothing
        , returning  = Projection $ d_docId . d_table
        }
      pure $ Left doc


quickAcceptableDBUri :: Expr T.Text -> Expr Bool
quickAcceptableDBUri uri = do
  let paths =
        foldr1 (||.) $ do
          z <- forbidPaths
          pure $ like (lit $ T.pack $ "%" <> z <> "%") uri
      sites =
        foldr1 (||.) $ do
          z <- forbidSites
          pure $ like (lit $ T.pack $ "%" <> z <> "/%") uri
  not_ $ paths ||. sites


discover :: HasCallStack => Connection -> Discovery Identity -> IO ()
discover conn disc = do
  mcandl <- getCanonicalUri (unsafeURI $ T.unpack $ disc_uri disc)
  putStrLn $ "canonical: " <> show (fmap snd mcandl)
  let mark x = markDiscovered conn x $ \d -> disc_id d ==. lit (disc_id disc)

  case mcandl of
    Nothing -> mark Nothing
    Just (dl, can) -> do
      ed <- getDocByCanonicalUri conn can
      case ed of
        Right d -> do
          putStrLn "already indexed"
          mark $ Just $ d_docId d

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
          writeFilestore fs
          mark $ Just did
          reindex conn did fs


markDiscovered :: Connection -> Maybe DocId -> (Discovery Expr -> Expr Bool) -> IO ()
markDiscovered conn mdoc f = doUpdate_ conn $
    Update
    { target = discoverySchema
    , from = pure ()
    , set = const $ \d ->
        case mdoc of
          Nothing -> d { disc_dead = lit True }
          Just docid -> d { disc_canonical = lit $ Just docid }
    , updateWhere = const f
    , returning = pure ()
    }


reindex :: HasCallStack => Connection -> DocId -> Filestore -> IO ()
reindex conn did fs = void $ tryIO $ do
  let uri = fs_uri fs
      !tree = parsePermissiveTree $ decodeUtf8 $ fs_data fs
      runL
        = runScraper
        $ fromMaybe (error "failed to parse the html document!") tree

  (dom, directives) <- getDomain conn uri
  let can_index = checkRobotsDirectives directives (CanIndex uri)

  Right inc_depth
    <- fmap (fmap (fromMaybe 0 . listToMaybe))
     $ doSelect conn
     $ incomingDepth did
  Right dist'
    <- fmap (fmap $ fmap (+1)
                  . fromMaybe nullDist
                  . listToMaybe
                  . sortOn (minimum . (maxBound:) . catMaybes . getDistance)
            )
     $ doSelect conn
     $ incomingDistance did
  let depth' = inc_depth + 1

  Just !xpol <- evaluate $ runL $ isSpiritualPollution uri
  let pol = mconcat
              [ flagIf DisallowedByRobots $ not can_index
              , flagIf IsProhibitedURI $ not $ isAcceptableLink uri
              , xpol
              ]
  Just !feats  <- evaluate $ runL features
  print (pol, feats)

  update_doc <-
    case can_index of
      False -> pure id
      True -> do
        Just !rls <- evaluate $ runL links
        let ls = fmap (normalizeURI . flip relativeTo uri) $ S.toList rls

        when (isEmpty pol) $
          insertEdges conn did depth' dist' ls

        Just !ts <- evaluate $ runL title
        t <- buildTitleSegs conn did ts

        Just !pc <- evaluate $ runL rankContent
        Just !( (script_uris, script_size)
              , (style_uris, style_size)
              , tweet_count
              , img_count
              ) <- evaluate $ runL $ (,,,) <$> scriptAssets uri <*> styleAssets uri <*> tweets <*> gifs
        more_scripts <- getAssetSizes conn $ fmap (T.pack . show) script_uris
        more_styles  <- getAssetSizes conn $ fmap (T.pack . show) style_uris

        let (stats :: PageStats Identity) = PageStats
              { ps_js      = fromIntegral $ script_size + sum more_scripts
              , ps_css     = fromIntegral $ style_size  + sum more_styles
              , ps_tweets  = fromIntegral $ tweet_count
              , ps_gifs    = fromIntegral $ img_count
              , ps_cookies = False
              }

        let word_count = length . T.words
            num_words = sum
              [ word_count ts
              , word_count $ pc_headings pc
              , word_count $ pc_content pc
              , word_count $ pc_comments pc
              ]

        pure $ \d -> d
          { d_table = (d_table d)
            { d_title     = lit t
            , d_wordCount = lit $ fromIntegral num_words
            , d_stats     = lit stats
            , d_distance  = lit dist'
            }
          , d_doc_text = lit $ T.intercalate " "
              [ ts
              , pc_headings pc
              , pc_content pc
              , pc_comments pc
              ]
          , d_search = lit $ Tsvector
              [ (A, ts)
              , (B, pc_headings pc)
              , (C, pc_content pc)
              , (D, pc_comments pc)
              ]
          }

  doUpdate_ conn $ Update
    { target = documentSchema'
    , from = pure ()
    , set = const $ \d ->
        let d' = update_doc d
         in d'
           { d_table = (d_table d')
              { d_domain = lit $ Just dom
              , d_features = lit feats
              , d_flags = lit pol
              }
           }
    , updateWhere = const $ \d -> d_docId (d_table d) ==. lit did
    , returning = pure ()
    }


insertEdges :: Connection -> DocId -> Int32 -> Distance Int16 -> [URI] -> IO ()
insertEdges conn did depth dist ls = do
  Right discs <- doInsert conn $ Insert
    { into = discoverySchema
    , onConflict = DoNothing
    , returning = Projection disc_id
    , rows = do
        discid <- nextDiscId
        dst <- values $ fmap (lit . T.pack . show) ls
        pure $ (lit emptyDiscovery)
          { disc_id       = discid
          , disc_uri      = dst
          , disc_depth    = lit depth
          , disc_distance = lit dist
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


spiderMain :: HasCallStack => Maybe Int -> IO ()
spiderMain threads = do
  Right conn <- connect
  z <- doInsert conn rootNodes
  print z
  ts <- for [0 .. fromMaybe 0 threads] $ const $ do
    async $ do
      forever $ do
        Right [disc] <- doSelect conn nextToExplore

        let url = disc_uri disc
        putStrLn $ "fetching " <> T.unpack url
        catch
          (do
            Just uri <- evaluate $ parseURI $ T.unpack url
            case isAcceptableLink uri of
              False -> do
                putStrLn "unacceptable"
                markDead conn (disc_id disc)
              True ->
                discover conn disc
          )
          (\SomeException{} -> do
            putStrLn "failed"
            markDead conn (disc_id disc)
          )
  void $ for_ ts wait

