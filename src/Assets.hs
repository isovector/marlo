module Assets where

import           Control.Monad (void)
import           DB
import qualified Data.ByteString.Char8 as BS
import           Data.Int (Int64)
import qualified Data.Set as S
import           Data.Text (Text)
import           Data.Traversable (for)
import           Network.HTTP.Client (responseHeaders)
import           Network.HTTP.Types (hContentLength)
import           Network.HttpUtils (doHEADRequest)
import           Rel8


getAssetSizes :: Connection -> [Text] -> IO [Int64]
getAssetSizes conn uris = do
  Right szs <-
    doSelect conn $ do
      a <- each assetSchema
      where_ $ in_ (a_uri a) $ fmap lit uris
      pure a
  let missing = S.fromList uris S.\\ S.fromList (fmap a_uri szs)
  rest <-
    for (S.toList missing) $ \turi -> do
      resp <- doHEADRequest turi
      let size = maybe 0 (fromIntegral . BS.length)
               $ lookup hContentLength
               $ responseHeaders resp
      void $ doInsert conn $ Insert
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

