{-

CREATE FUNCTION array_zip_with_least( arr_q anyarray, arr_e anyarray )
RETURNS anyarray
AS $$
  SELECT ARRAY(
    SELECT least(a, b)
    FROM unnest(
      arr_q, -- ex1
      arr_e  -- ex2
    ) AS t(a,b)
  );
$$ LANGUAGE sql
IMMUTABLE;

CREATE FUNCTION array_zip_with_lt( arr_q anyarray, arr_e anyarray )
RETURNS boolean[]
AS $$
  SELECT ARRAY(
    SELECT a < b
    FROM unnest(
      arr_q, -- ex1
      arr_e  -- ex2
    ) AS t(a,b)
  );
$$ LANGUAGE sql
IMMUTABLE;

CREATE FUNCTION array_all_true( arr_q anyarray )
RETURNS boolean
AS $$
    SELECT bool_and(a)
    FROM unnest(
      arr_q -- ex1
    ) AS t(a);
$$ LANGUAGE sql
IMMUTABLE;


CREATE FUNCTION array_inc( arr_q anyarray )
RETURNS anyarray
AS $$
  SELECT ARRAY(
    SELECT a + 1
    FROM unnest(
      arr_q -- ex1
    ) AS t(a)
  );
$$ LANGUAGE sql
IMMUTABLE;


-}

module DB
  ( module DB
  , module DB.Asset
  , module DB.Document
  , module DB.Discovery
  , module DB.Domain
  , module DB.Edges
  , module DB.PageContent
  , module DB.PageRawData
  , module DB.PageStats
  , module DB.SearchResult
  , module DB.Titles
  , module Rel8.Machinery

  , nullDist
  , numRootSites

  , acquire
  , Connection
  , QueryError
  , Identity
  ) where

import Config
import DB.Asset
import DB.Discovery
import DB.Document
import DB.Domain
import DB.Edges
import DB.PageContent
import DB.PageRawData
import DB.PageStats
import DB.RootSites
import DB.SearchResult
import DB.Titles
import Data.Function (on)
import Data.Functor.Identity (Identity)
import Hasql.Connection (settings, Connection, acquire, ConnectionError)
import Hasql.Session (QueryError)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Rel8.Machinery


rootNodes :: Insert ()
rootNodes = Insert
  { into = discoverySchema
  , rows = do
      d <- nextDiscId
      z <- values rootSites
      pure $ (lit emptyDiscovery)
        { disc_id  = d
        , disc_uri = z
        }
  , onConflict = DoUpdate $ Upsert
      { index = disc_uri
      , set = \new old -> old { disc_distance = disc_distance new }
      , updateWhere = on (==.) disc_uri
      }
  , returning = pure ()
  }


connect :: IO (Either ConnectionError Connection)
connect = acquire $ settings cfg_pg_host cfg_pg_port cfg_pg_user cfg_pg_pass "db"

