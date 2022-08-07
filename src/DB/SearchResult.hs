module DB.SearchResult where

import DB.PageStats
import Data.Functor.Identity
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Types


data SearchResult f = SearchResult
  { sr_ranking    :: !(Column f Float)
  , sr_id         :: !(Column f DocId)
  , sr_uri        :: !(Column f Text)
  , sr_title      :: !(Column f Text)
  , sr_popularity :: !(Column f (Maybe Int32))
  , sr_stats      :: !(PageStats f)
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Eq (SearchResult Identity)
deriving instance Show (SearchResult Identity)

