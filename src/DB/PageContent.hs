module DB.PageContent where

import Data.Functor.Identity
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)


data PageContent f = PageContent
  { pc_headings :: Column f Text
  , pc_content  :: Column f Text
  , pc_comments :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Eq (PageContent Identity)
deriving instance Show (PageContent Identity)

