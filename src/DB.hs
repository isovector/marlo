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


-- find path
with recursive graph_cte (src, dst, id)
as
(
  select src, dst, id
  from edges
  where dst = 572908
  union all
  select prev.src, prev.dst, prev.id
  from edges prev
    join graph_cte next on prev.dst = next.src
)
select id, src, dst
from graph_cte
order by id;

-}

module DB
  ( module DB
  , module DB.Asset
  , module DB.Document
  , module DB.Edges
  , module DB.PageContent
  , module DB.PageRawData
  , module DB.PageStats
  , module DB.SearchResult
  , nullDist
  , numRootSites
  ) where

import Config
import DB.Asset
import DB.Document
import DB.Edges
import DB.PageContent
import DB.PageRawData
import DB.PageStats
import DB.RootSites
import DB.SearchResult
import Hasql.Connection (Settings, settings)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Rel8.Arrays (insertAt')
import Types


connectionSettings :: Settings
connectionSettings =
  settings cfg_pg_host cfg_pg_port cfg_pg_user cfg_pg_pass "db"


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

