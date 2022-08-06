module DB.PageRawData where

import Data.ByteString (ByteString)
import Data.Functor.Identity
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Rel8.Headers

data PageRawData f = PageRawData
  { prd_data    :: Column f ByteString
  , prd_headers :: Column f [Header]
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Show (PageRawData Identity)

