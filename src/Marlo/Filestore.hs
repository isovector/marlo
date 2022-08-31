module Marlo.Filestore where

import Config
import Types
import qualified Data.ByteString as BS
import Data.Serialize (encode)
import System.FilePath ((</>))
import Data.Hashable (hash)


writeFilestore :: Filestore -> IO ()
writeFilestore fs =
  BS.writeFile (cfg_filestore </> show (hash fs)) $ encode fs

