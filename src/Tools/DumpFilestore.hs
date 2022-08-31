module Tools.DumpFilestore where

import           DB
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Marlo.Filestore (writeFilestore)
import           Network.URI (parseURI)
import           Rel8
import           Types
import           Utils (withDocuments)


main :: IO ()
main = do
  Right conn <- connect
  now <- getCurrentTime

  withDocuments conn (\d -> d_state d ==. lit Explored) $ \doc -> do
    print $ d_uri doc
    writeFilestore $ Filestore
      { fs_uri       = fromJust $ parseURI $ T.unpack $ d_uri doc
      , fs_collected = now
      , fs_headers   = prd_headers $ d_raw doc
      , fs_data      = prd_data $ d_raw doc
      }

