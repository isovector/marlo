{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Index where

import qualified Streaming.Prelude as S
import Data.Warc
import Streaming.Pipes (fromStreamingByteString, toStream)
import qualified Streaming.ByteString as BSS
import Control.Monad (void)
import Data.ByteString.Streaming.HTTP (runResourceT, ResourceT)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client (parseRequest)
import Data.Maybe (fromJust)
import Network.HTTP (rspHeaders, parseRequestHead, parseResponseHead, RequestData)
import Data.Char (isSpace)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Bifunctor (bimap, first)
import Network.HTTP.Headers
import Data.Tuple (swap)


main :: IO ()
main = runResourceT $ do
  let warc = parseWarc $ fromStreamingByteString $ BSS.readFile "/home/sandy/CC-MAIN-20220516041337-20220516071337-00000.warc"
  void $ iterRecords withRec warc


splitReq :: ByteString -> (ByteString, ByteString)
splitReq
  = bimap (BS8.unlines) (BS8.unlines)
  . break (BS.null . BS.filter (not . isSpace))
  . BS8.lines

instance HasHeaders [Header] where
  getHeaders = id
  setHeaders = const


yo :: Monad m => S.Stream (S.Of ByteString) m r -> S.Stream (S.Of ([Header], ByteString)) m r
yo = S.map (first $ \(_, _, h) -> h)
   . S.filter ((\(_, w, _) -> show w == "200") . fst)
   . S.map swap
   . S.mapMaybe (traverse (hush . parseRequestHead . lines . BS.unpack) . swap . splitReq)

hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right b) = Just b

unsafeGetWarcHeader r = either (const $ error "fromRight") id . fromJust . lookupField r


withRec :: MonadIO m => Record m a -> m a
withRec rec = do
  case lookupField (recHeader rec) contentType of
    Just (Right "application/http; msgtype=response") -> do
      flip S.mapM_ (yo $ toStream $ recContent rec) $ \(hs, body) -> do
          liftIO $ print $ (unsafeGetWarcHeader (recHeader rec) warcTargetUri, findHeader HdrContentType hs)
        -- error "yo"
    _ -> flip S.mapM_ (toStream $ recContent rec) $ const $ pure ()

