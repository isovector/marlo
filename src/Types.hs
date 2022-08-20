module Types where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.URI
import Rel8 (DBType, DBEq, DBOrd, ReadShow(..))
import Servant (FromHttpApiData, parseQueryParam, ToHttpApiData, toQueryParam)
import Text.HTML.Scalpel


data Env = Env
  { e_uri  :: URI
  , e_mgr  :: Manager
  , e_conn :: Connection
  }

type Ranker = ScraperT Text (ReaderT Env IO)


data Link a = Link
  { l_text :: Text
  , l_uri :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Download f a = Download
  { d_mime :: f ByteString
  , d_headers :: ResponseHeaders
  , d_body :: a
  }
  deriving (Functor)

sequenceDownload :: ByteString -> Download Maybe a -> Download Identity a
sequenceDownload bs d@(Download m _ _) =
  d { d_mime = Identity $ fromMaybe bs m }

data Search a
  = Term a
  | Phrase [a]
  | Negate (Search a)
  | And (Search a) (Search a)
  | Or (Search a) (Search a)
  | SiteLike Text
  | WithProperty SiteProp Predicate
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data SiteProp
  = JSBundle
  | CSSBundle
  deriving (Eq, Ord, Show)

data Predicate
  = Exactly Int
  | LessThan Int
  | GreaterThan Int
  deriving (Eq, Ord, Show)


data SearchVariety
  = Traditional
  | Spatial
  deriving (Eq, Ord, Show, Prelude.Enum, Bounded)

instance ToHttpApiData SearchVariety where
  toQueryParam Traditional = "traditional"
  toQueryParam Spatial = "spatial"

instance FromHttpApiData SearchVariety where
  parseQueryParam "traditional" = Right Traditional
  parseQueryParam "spatial"     = Right Spatial
  parseQueryParam _             = Left "SearchVariety must be one of 'traditional' or 'spatial'"


newtype EdgeId = EdgeId
  { unEdgeId :: Int64
  }
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)


newtype DocId = DocId
  { unDocId :: Int64
  }
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)


newtype DomainId = DomainId
  { unDomainId :: Int64
  }
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)


newtype TitleEdgeId = TitleEdgeId
  { unTitleEdgeId :: Int64
  }
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)


newtype TitleSegId = TitleSegId
  { unTitleSegId :: Int64
  }
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)


data DocumentState
  = Discovered
  | Explored
  | Pruned
  | Errored
  | Unacceptable
  | NoContent
  | DisallowedByRobots
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (DBType, DBEq) via ReadShow DocumentState


data LimitStrategy
  = Limit Word
  | Paginate Word  -- ^ Page size
  deriving stock (Eq, Ord, Show, Read, Generic)


newtype PageNumber = PageNumber
  { getPageNumber :: Word
  } deriving newtype (Eq, Ord, Show, Num, Enum, Bounded, ToHttpApiData, FromHttpApiData)


data RobotDirectives = RobotDirectives
  { rb_allow :: [Text]
  , rb_disallow :: [Text]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (DBType, DBEq) via ReadShow RobotDirectives

instance Semigroup RobotDirectives where
  (RobotDirectives txts txts') <> (RobotDirectives txts2 txts3)
    = RobotDirectives
        {rb_allow = txts <> txts2, rb_disallow = txts' <> txts3}

instance Monoid RobotDirectives where
  mempty = RobotDirectives {rb_allow = mempty, rb_disallow = mempty}

