{-

CREATE SEQUENCE disc_id_seq;

CREATE TABLE IF NOT EXISTS discovery (
  id int8 PRIMARY KEY,
  uri TEXT UNIQUE NOT NULL,
  depth int4 NOT NULL,
  distance int2[] NOT NULL,
  dead bool NOT NULL,
  resolved_at timestamptz,
  canonical int8 REFERENCES documents(id) ON DELETE CASCADE
);

ALTER TABLE discovery ADD COLUMN distance int2[] not null default(array[0,0,0,0,0,0,0,0]);

CREATE INDEX disc_uri_idx ON discovery (uri);
CREATE INDEX disc_dead_idx ON discovery (dead);
CREATE INDEX disc_canonical_idx ON discovery (canonical);

-}

module DB.Discovery where

import DB.RootSites (nullDist)
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Types


data Discovery f = Discovery
  { disc_id         :: Column f DiscId
  , disc_uri        :: Column f Text
  , disc_depth      :: Column f Int32
  , disc_distance   :: Column f (Distance Int16)
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
    { disc_id         = "id"
    , disc_uri        = "uri"
    , disc_depth      = "depth"
    , disc_distance   = "distance"
    , disc_dead       = "dead"
    , disc_resolvedAt = "resolved_at"
    , disc_canonical  = "canonical"
    }
  }


emptyDiscovery :: Discovery Identity
emptyDiscovery = Discovery
  { disc_id         = DiscId 0
  , disc_uri        = ""
  , disc_depth      = 0
  , disc_distance   = nullDist
  , disc_dead       = False
  , disc_resolvedAt = Nothing
  , disc_canonical  = Nothing
  }


