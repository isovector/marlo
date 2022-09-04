module Marlo.Filestore where

import           Codec.Compression.Zlib
import           Config
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable (hash)
import           Data.Serialize (encode, decode)
import           Streaming (Stream, Of)
import qualified Streaming as S
import qualified Streaming.Prelude  as S
import           System.FilePath ((</>))
import           Types
import System.Directory (listDirectory)


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


streamFilestore :: Stream (Of Filestore) IO ()
streamFilestore
  = S.mapMaybe hush
  . S.mapM readFilestore
  . S.map (cfg_filestore </>)
  $ S.effect (fmap S.each $ listDirectory cfg_filestore)

