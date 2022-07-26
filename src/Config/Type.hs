module Config.Type where

import Data.ByteString (ByteString)
import Data.Word (Word16)

data Config = Config
  { cfg_port :: Int
  , cfg_pg_host :: ByteString
  , cfg_pg_port :: Word16
  , cfg_pg_user :: ByteString
  , cfg_pg_pass :: ByteString
  }
  deriving (Eq, Ord, Show, Read)

