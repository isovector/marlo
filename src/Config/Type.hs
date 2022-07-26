module Config.Type where

data Config = Config
  { cfg_port :: Int
  }
  deriving (Eq, Ord, Show, Read)
