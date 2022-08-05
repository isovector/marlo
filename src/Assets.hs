module Assets where

import           Control.Monad (void)
import           DB
import qualified Data.ByteString.Char8 as BS
import           Data.Int (Int64)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Hasql.Connection (Connection)
import           Hasql.Session (run, statement)
import           Network.HTTP.Client (httpNoBody, Manager, parseRequest, responseHeaders)
import           Network.HTTP.Types (hContentLength)
import           Rel8


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
      let size = maybe 0 (fromIntegral . BS.length)
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

