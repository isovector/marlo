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
  , module DB.Domain
  , module DB.Edges
  , module DB.PageContent
  , module DB.PageRawData
  , module DB.PageStats
  , module DB.SearchResult
  , module DB.Titles

  , nullDist
  , numRootSites

  , acquire
  , Connection
  , QueryError
  , Identity
  ) where

import Config
import DB.Asset
import DB.Document
import DB.Domain
import DB.Edges
import DB.PageContent
import DB.PageRawData
import DB.PageStats
import DB.RootSites
import DB.SearchResult
import DB.Titles
import Data.Functor.Identity (Identity)
import Hasql.Connection (settings, Connection, acquire, ConnectionError)
import Hasql.Session (run, statement, QueryError)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Rel8.Arrays (insertAt')
import Types



rootNodes :: Insert ()
rootNodes = Insert
  { into = documentSchema
  , rows = do
      d <- nextDocId
      (idx, z) <- values $ zip (fmap lit [0..]) rootSites
      pure $ (lit emptyDoc)
        { d_docId    = d
        , d_uri      = z
        , d_state    = lit Discovered
        , d_depth    = 0
        , d_distance = insertAt' (lit $ fromIntegral numRootSites) idx $ nullify 0
        }
  , onConflict = DoUpdate Upsert
      { index = d_uri
      , set = \new old -> old { d_distance = d_distance new }
      , updateWhere = \new old -> d_uri new ==. d_uri old
      }
  , returning = pure ()
  }


doInsert :: Connection -> Insert a -> IO (Either QueryError a)
doInsert conn = flip run conn . statement () . insert


doSelect
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Query exprs
    -> IO (Either QueryError [FromExprs exprs])
doSelect conn = flip run conn . statement () . select


doUpdate :: Connection -> Update a -> IO (Either QueryError a)
doUpdate conn = flip run conn . statement () . update

doDelete :: Connection -> Delete a -> IO (Either QueryError a)
doDelete conn = flip run conn . statement () . delete


connect :: IO (Either ConnectionError Connection)
connect = acquire $ settings cfg_pg_host cfg_pg_port cfg_pg_user cfg_pg_pass "db"

