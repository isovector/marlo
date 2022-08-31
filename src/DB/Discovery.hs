{-

CREATE SEQUENCE disc_id_seq;

CREATE TABLE IF NOT EXISTS discovery (
  id int8 PRIMARY KEY,
  uri TEXT UNIQUE NOT NULL,
  depth int4 NOT NULL,
  dead bool NOT NULL,
  resolved_at timestamptz,
  canonical int8
);

-}

module DB.Discovery where

import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Types
import Data.Time (UTCTime)


data Discovery f = Discovery
  { disc_id         :: Column f DiscId
  , disc_uri        :: Column f Text
  , disc_depth      :: Column f Int32
  , disc_dead       :: Column f Bool
  , disc_resolvedAt :: Column f (Maybe UTCTime)
  , disc_canonical  :: Column f (Maybe DocId)
  }
  deriving stock Generic
  deriving anyclass Rel8able


nextDiscId :: Query (Expr DiscId)
nextDiscId = fmap coerce $ pure $ nextval "disc_id_seq"


discoverySchema :: TableSchema (Discovery Name)
discoverySchema = TableSchema
  { name    = "discovery"
  , schema  = Just "public"
  , columns = Discovery
    { disc_id = "id"
    , disc_uri = "uri"
    , disc_depth = "depth"
    , disc_dead = "dead"
    , disc_resolvedAt = "resolved_at"
    , disc_canonical = "canonical"
    }
  }


emptyDiscovery :: Discovery Identity
emptyDiscovery = Discovery
  { disc_id         = DiscId 0
  , disc_uri        = ""
  , disc_depth      = 0
  , disc_dead       = False
  , disc_resolvedAt = Nothing
  , disc_canonical  = Nothing
  }


