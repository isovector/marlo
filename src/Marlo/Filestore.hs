module Marlo.Filestore where

import           Codec.Compression.Zlib
import           Config
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable (hash)
import           Data.Serialize (encode, decode)
import           System.FilePath ((</>))
import           Types


writeFilestore :: Filestore -> IO ()
writeFilestore fs
  = BSL.writeFile (cfg_filestore </> "doc" <> show (hash fs))
  $ compress
  $ BSL.fromStrict
  $ encode fs


readFilestore :: FilePath -> IO (Either String Filestore)
readFilestore
  = fmap (decode . BSL.toStrict . decompress)
  . BSL.readFile

