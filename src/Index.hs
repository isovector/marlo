{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Index where

import           Control.Exception (SomeException(SomeException), catch)
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           DB
import           DB (connectionSettings)
import           Data.Bifunctor (bimap, first, second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Streaming.HTTP (runResourceT, ResourceT)
import           Data.Char (isSpace)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Functor.Contravariant ((>$<))
import           Data.Functor.Identity (Identity)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Tuple (swap)
import           Data.Warc
import           Hasql.Connection (acquire, Connection)
import           Hasql.CursorTransactionIO.TransactionIO (cursorTransactionIO)
import           Hasql.Session
import           Hasql.Streaming (streamingQuery)
import           Hasql.TransactionIO.Sessions
import           Network.HTTP (rspHeaders, parseRequestHead, parseResponseHead, RequestData)
import           Network.HTTP.Client (parseRequest)
import           Network.HTTP.Headers
import           Network.URI (parseURI)
import           Rel8
import           Signals (title)
import           Spider (index, indexFromDB)
import qualified Streaming.ByteString as BSS
import           Streaming.Pipes (fromStreamingByteString, toStream)
import qualified Streaming.Prelude as S
import           Utils (unsafeURI, runRanker, paginate)
import qualified Network.HTTP.Client.TLS as HTTP
import Types



main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  for_ [0 .. 17408] $ \page -> do
    Right docs <-
      flip run conn $ statement () $ select $ paginate 100 page $ orderBy (d_docId >$< asc) $ do
            d <- each discoverySchema
            where_ $ d_state d ==. lit Explored
            pure d
    for_ docs $ \doc -> do
      catch (updateTitle conn doc) $ \(SomeException _) -> do
        putStrLn "errored ^"


updateTitle :: Connection -> Discovery Identity -> IO ()
updateTitle conn disc = do
  let uri = unsafeURI $ T.unpack $ d_uri disc
  mgr <- HTTP.getGlobalManager
  Just t <- runRanker (Env uri mgr conn) (T.decodeUtf8 $ d_data disc) title
  print $ (uri, t)
  void $ flip run conn $ statement () $ update $
    Update
      { target = discoverySchema
      , from = pure ()
      , set = \_ d -> d { d_title = lit t }
      , updateWhere = \_ d -> d_docId d ==. lit (d_docId disc)
      , returning = pure ()
      }




-- warcIndexMain :: IO ()
-- warcIndexMain = do
--   Right conn <- acquire connectionSettings
--   runResourceT $ do
--     let warc = parseWarc $ fromStreamingByteString $ BSS.readFile "/home/sandy/CC-MAIN-20220516041337-20220516071337-00000.warc"
--     void $ iterRecords (withRec conn) warc


-- splitReq :: ByteString -> (ByteString, ByteString)
-- splitReq
--   = bimap (BS8.unlines) (BS8.unlines)
--   . break (BS.null . BS.filter (not . isSpace))
--   . BS8.lines

-- instance HasHeaders [Header] where
--   getHeaders = id
--   setHeaders = const


-- yo :: Monad m => S.Stream (S.Of ByteString) m r -> S.Stream (S.Of ([Header], Text)) m r
-- yo = S.mapMaybe (hush . sequenceA . second T.decodeUtf8')
--    . S.map (first $ \(_, _, h) -> h)
--    . S.filter ((\(_, w, _) -> show w == "200") . fst)
--    . S.map swap
--    . S.mapMaybe (traverse (hush . parseRequestHead . lines . BS.unpack) . swap . splitReq)

-- hush :: Either a b -> Maybe b
-- hush (Left _) = Nothing
-- hush (Right b) = Just b

-- unsafeGetWarcHeader :: RecordHeader -> Data.Warc.Field c -> c
-- unsafeGetWarcHeader r
--   = either (const $ error "fromRight") id
--   . fromJust
--   . lookupField r


-- withRec :: MonadIO m => Connection -> Record m a -> m a
-- withRec conn rec = do
--   case lookupField (recHeader rec) contentType of
--     Just (Right "application/http; msgtype=response") -> do
--       flip S.mapM_ (yo $ toStream $ recContent rec) $ \(hs, body) -> liftIO $ do
--         for_ (parseURI $ (\ (Uri uri) -> BS.unpack uri) $ unsafeGetWarcHeader (recHeader rec) warcTargetUri) $ \uri ->
--           for_ (findHeader HdrContentType hs) $ \mime ->
--             Spider.index conn 100 uri (BS.pack mime) body
--         -- liftIO $ print $ (unsafeGetWarcHeader (recHeader rec) warcTargetUri, findHeader HdrContentType hs)
--         -- error "yo"
--     _ -> flip S.mapM_ (toStream $ recContent rec) $ const $ pure ()

