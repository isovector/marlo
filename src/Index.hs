{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall #-}

module Index where

import Control.Exception
import Control.Monad (void, forever)
import DB
import Hasql.Connection (acquire)
import Hasql.Session
import Rel8
import Spider (indexFromDB)
import Utils (random)


main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  forever $ do
    Right [doc] <-
      flip run conn $ statement () $ select $ limit 1 $ orderBy random $ do
            d <- each discoverySchema
            where_ $ d_state d ==. lit Explored &&. d_content d ==. lit ""
            pure d
    print $ d_uri doc
    catch (indexFromDB conn doc) $ \(SomeException _) -> do
      putStrLn "errored ^"
      void $ flip run conn $ statement () $ update $ Update
        { target = discoverySchema
        , from = pure ()
        , set = \_ d -> fixSearch $ d { d_state = lit NoContent }
        , updateWhere = \_ d -> d_docId d ==. lit (d_docId doc)
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

