{-

CREATE TABLE IF NOT EXISTS assets (
  uri TEXT PRIMARY KEY NOT NULL,
  size int8 NOT NULL
);

-}

module DB.Asset where

import Data.Functor.Identity
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)


data Asset f = Asset
  { a_uri   :: Column f Text
  , a_size :: Column f Int64
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Eq (Asset Identity)
deriving instance Show (Asset Identity)


assetSchema :: TableSchema (Asset Name)
assetSchema = TableSchema
  { name    = "assets"
  , schema  = Just "public"
  , columns = Asset
      { a_uri   = "uri"
      , a_size = "size"
      }
  }

