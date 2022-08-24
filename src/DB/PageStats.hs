module DB.PageStats where

import Data.Functor.Identity
import Data.Int (Int32, Int16)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)


data PageStats f = PageStats
  { ps_js      :: Column f Int32
  , ps_css     :: Column f Int32
  , ps_tweets  :: Column f Int16
  , ps_gifs    :: Column f Int16
  , ps_cookies :: Column f Bool
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Show (PageStats Identity)
deriving instance Eq (PageStats Identity)
deriving instance Ord (PageStats Identity)

