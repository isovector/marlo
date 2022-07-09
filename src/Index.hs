{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Index where

import           Control.Monad (void)
import           Control.Monad.IO.Class
import           DB (connectionSettings)
import           Data.Bifunctor (bimap, first, second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Streaming.HTTP (runResourceT, ResourceT)
import           Data.Char (isSpace)
import           Data.Foldable (for_)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Tuple (swap)
import           Data.Warc
import           Hasql.Connection (acquire, Connection)
import           Network.HTTP (rspHeaders, parseRequestHead, parseResponseHead, RequestData)
import           Network.HTTP.Client (parseRequest)
import           Network.HTTP.Headers
import           Network.URI (parseURI)
import           Spider (index)
import qualified Streaming.ByteString as BSS
import           Streaming.Pipes (fromStreamingByteString, toStream)
import qualified Streaming.Prelude as S


main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  runResourceT $ do
    let warc = parseWarc $ fromStreamingByteString $ BSS.readFile "/home/sandy/CC-MAIN-20220516041337-20220516071337-00000.warc"
    void $ iterRecords (withRec conn) warc


splitReq :: ByteString -> (ByteString, ByteString)
splitReq
  = bimap (BS8.unlines) (BS8.unlines)
  . break (BS.null . BS.filter (not . isSpace))
  . BS8.lines

instance HasHeaders [Header] where
  getHeaders = id
  setHeaders = const


yo :: Monad m => S.Stream (S.Of ByteString) m r -> S.Stream (S.Of ([Header], Text)) m r
yo = S.mapMaybe (hush . sequenceA . second T.decodeUtf8')
   . S.map (first $ \(_, _, h) -> h)
   . S.filter ((\(_, w, _) -> show w == "200") . fst)
   . S.map swap
   . S.mapMaybe (traverse (hush . parseRequestHead . lines . BS.unpack) . swap . splitReq)

hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right b) = Just b

unsafeGetWarcHeader :: RecordHeader -> Field c -> c
unsafeGetWarcHeader r
  = either (const $ error "fromRight") id
  . fromJust
  . lookupField r


withRec :: MonadIO m => Connection -> Record m a -> m a
withRec conn rec = do
  case lookupField (recHeader rec) contentType of
    Just (Right "application/http; msgtype=response") -> do
      flip S.mapM_ (yo $ toStream $ recContent rec) $ \(hs, body) -> liftIO $ do
        for_ (parseURI $ (\ (Uri uri) -> BS.unpack uri) $ unsafeGetWarcHeader (recHeader rec) warcTargetUri) $ \uri ->
          for_ (findHeader HdrContentType hs) $ \mime ->
            index conn uri (BS.pack mime) body
        -- liftIO $ print $ (unsafeGetWarcHeader (recHeader rec) warcTargetUri, findHeader HdrContentType hs)
        -- error "yo"
    _ -> flip S.mapM_ (toStream $ recContent rec) $ const $ pure ()

