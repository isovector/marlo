{-# OPTIONS_GHC -Wno-orphans #-}

module Types.Orphans where

import GHC.Generics (Generic)
import Data.Serialize
import Data.Time
import Network.URI
import Network.HTTP
import Data.Function (on)
import Data.Hashable (Hashable, hashWithSalt)


instance Serialize DiffTime where
  put = put . diffTimeToPicoseconds
  get = fmap picosecondsToDiffTime get

instance Hashable DiffTime where
  hashWithSalt s = hashWithSalt s . diffTimeToPicoseconds

instance Eq Header where
  (==) = on (==) show

deriving stock instance Generic Day
deriving stock instance Generic UTCTime
deriving anyclass instance Serialize Day
deriving anyclass instance Serialize UTCTime
deriving anyclass instance Hashable UTCTime
deriving anyclass instance Hashable Day

deriving anyclass instance Serialize URI
deriving anyclass instance Serialize URIAuth
deriving anyclass instance Hashable URI
deriving anyclass instance Hashable URIAuth

deriving stock instance Generic HeaderName
deriving anyclass instance Serialize HeaderName
deriving anyclass instance Hashable HeaderName

deriving stock instance Generic Header
deriving anyclass instance Hashable Header
deriving anyclass instance Serialize Header

