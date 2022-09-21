{-# OPTIONS_GHC -Wno-orphans #-}

module Types
  ( module Types
  , Int16
  , Int32
  , Int64
  ) where

import           Control.DeepSeq (NFData)
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Coerce (Coercible, coerce)
import           Data.Functor.Identity
import           Data.Hashable (Hashable)
import           Data.Int (Int64, Int16, Int32)
import           Data.Maybe (fromMaybe)
import           Data.Serialize
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime(..))
import           GHC.Generics (Generic)
import           Hasql.Connection (Connection)
import           Network.HTTP (Header)
import           Network.HTTP.Types.Header (ResponseHeaders)
import           Network.URI
import           Rel8 (DBType, DBEq, DBOrd, ReadShow(..))
import           Servant (FromHttpApiData, parseQueryParam, ToHttpApiData, toQueryParam)
import           Text.HTML.Scalpel
import           Text.Read (readMaybe)
import           Types.Orphans ()


data Env = Env
  { e_uri  :: URI
  , e_conn :: Connection
  }

type Ranker = ScraperT Text (ReaderT Env IO)


data Download f a = Download
  { dl_mime :: f ByteString
  , dl_headers :: ResponseHeaders
  , dl_body :: a
  }
  deriving (Functor)

sequenceDownload :: ByteString -> Download Maybe a -> Download Identity a
sequenceDownload bs d@(Download m _ _) =
  d { dl_mime = Identity $ fromMaybe bs m }

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
  toQueryParam Traditional = "trad"
  toQueryParam Spatial     = "spatial"

instance FromHttpApiData SearchVariety where
  parseQueryParam "trad"    = Right Traditional
  parseQueryParam "spatial" = Right Spatial
  parseQueryParam _         = Left "SearchVariety must be one of 'trad' or 'spatial'"


newtype DiscId = DiscId
  { unDiscId :: Int64
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


data WindowSize = WindowSize
  { ws_width :: Int
  , ws_height :: Int
  }
  deriving (Eq, Ord, Show)

instance FromHttpApiData WindowSize where
  parseQueryParam
    = note "No size"
    . (parseWindowSize =<<)
    . fmap T.tail
    . lookup "size"
    . fmap (T.break (== '=') . T.strip)
    . T.split (== ';')

parseWindowSize :: Text -> Maybe WindowSize
parseWindowSize t = do
  let (tw, th) = T.break (== ',') $ T.strip t
  w <- readMaybe $ T.unpack tw
  h <- readMaybe $ T.unpack $ T.tail th
  pure $ WindowSize w h


note :: e -> Maybe a -> Either e a
note e Nothing = Left e
note _ (Just a) = Right a


data Filestore = Filestore
  { fs_uri :: URI
  , fs_collected :: UTCTime
  , fs_headers :: [Header]
  , fs_data :: ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (Serialize, Hashable)

data DocumentFlag
  = DisallowedByRobots
  | IsProhibitedURI
  | IsNews
  | IsShopping
  | IsPaywalled
  | IsListicle
  | IsMedia
  | IsAnticompetitiveTech
  | HasAds
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, NFData)

data DocumentFeature
  = HasMath
  | HasCode
  | IsForum
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, NFData)

------------------------------------------------------------------------------
-- | I'm so mad I need to write this every damn time
hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right b) = Just b


data SearchDimension
  = ByJavascript
  | ByCss
  | ByAssetSize
  | ByWordCount
  | ByRelevance
  | ByPopularity
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance FromHttpApiData SearchDimension where
  parseQueryParam "js"         = Right ByJavascript
  parseQueryParam "css"        = Right ByCss
  parseQueryParam "words"      = Right ByWordCount
  parseQueryParam "assets"     = Right ByAssetSize
  parseQueryParam "relevance"  = Right ByRelevance
  parseQueryParam "popularity" = Right ByPopularity
  parseQueryParam _
    = Left
    $ mappend "should be one of: "
    $ T.intercalate ", "
    $ fmap toQueryParam $ enumFrom @SearchDimension minBound

instance ToHttpApiData SearchDimension where
  toQueryParam ByJavascript = "js"
  toQueryParam ByCss        = "css"
  toQueryParam ByWordCount  = "words"
  toQueryParam ByAssetSize  = "assets"
  toQueryParam ByRelevance  = "relevance"
  toQueryParam ByPopularity = "popularity"


newtype Distance a = Distance
  { getDistance :: [Maybe a]
  }
  deriving stock (Eq, Ord, Show, Functor)
  deriving newtype (DBType, DBEq, DBOrd)


viewAs :: Coercible (f (Distance a)) (f [Maybe a]) => f (Distance a) -> f [Maybe a]
viewAs = coerce

