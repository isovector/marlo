{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Assets where

import qualified Data.Set as S
import Data.Set (Set)
import Rel8
import Data.Text (Text)
import Hasql.Connection (Connection)
import Data.Int (Int64)
import Control.Monad (void)
import Hasql.Session (run, statement)
import DB
import Data.Traversable (for)
import Network.HTTP.Client (httpNoBody, Manager, defaultRequest, Request (method), parseRequest, responseHeaders)
import Network.HTTP.Base (RequestMethod (HEAD))
import qualified Data.Text as T
import Network.URI (parseURI)
import Network.HTTP (mkRequest, lookupHeader)
import Network.HTTP.Types (hContentLength)
import Data.List (find)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Network.HTTP.Client.TLS as HTTP
import           Hasql.Connection (acquire, Connection)


getAssetSizes :: Manager -> Connection -> [Text] -> IO [Int64]
getAssetSizes mgr conn uris = do
  Right szs <-
    flip run conn $ statement () $ select $ do
      a <- each assetSchema
      where_ $ in_ (a_uri a) $ fmap lit uris
      pure a
  let missing = S.fromList uris S.\\ S.fromList (fmap a_uri szs)
  rest <-
    for (S.toList missing) $ \turi -> do
      req <- parseRequest $ "HEAD " <> T.unpack turi
      resp <- flip httpNoBody mgr req
      let size = maybe 0 (fromMaybe 0 . readMaybe . BS.unpack)
               $ lookup hContentLength
               $ responseHeaders resp
      void $ flip run conn $ statement () $ insert $ Insert
        { into = assetSchema
        , rows = pure $ Asset
            { a_uri  = lit turi
            , a_size = lit size
            }
        , onConflict = DoNothing
        , returning = pure ()
        }
      pure size
  pure $ fmap a_size szs <> rest



-- indexWords :: Connection -> DocId -> [(Int, Keyword)] -> IO ()
-- indexWords conn did pos = do
--   let kws = nubOrd $ fmap snd pos
--   void $ flip run conn $ statement () $ insert $ createWordIds kws
--   Right ws <- flip run conn $ statement () $ select $ getWordIds kws
--   let word_map = M.fromList $ ws <&> \(Words a b) -> (Keyword b, a)
--       pos' = fmap (second (word_map M.!)) pos
--   Right res <- flip run conn $ statement () $ insert $ insertKeywords did pos'
--   pure res

