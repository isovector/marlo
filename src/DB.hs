{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wall                   #-}

module DB where

import Config
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Int (Int64, Int32, Int16)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Connection (Settings, settings)
import Rel8 hiding (Enum)
import Rel8.Arrays (insertAt', arrayFill)
import Rel8.TextSearch
import Prelude hiding (null)
import Rel8.Headers


newtype EdgeId = EdgeId
  { unEdgeId :: Int64
  }
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)

newtype DocId = DocId
  { unDocId :: Int64
  }
  deriving newtype (Eq, Ord, Show, DBType, DBEq, DBOrd)

data DiscoveryState
  = Discovered
  | Explored
  | Pruned
  | Errored
  | Unacceptable
  | NoContent
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (DBType, DBEq) via ReadShow DiscoveryState

data Asset f = Asset
  { a_uri   :: Column f Text
  , a_size :: Column f Int64
  }
  deriving stock Generic
  deriving anyclass Rel8able

data Discovery f = Discovery
  { d_docId :: Column f DocId
  , d_uri   :: Column f Text
  , d_state :: Column f DiscoveryState
  , d_distance :: Column f [Maybe Int16]
  , d_depth :: Column f Int32
  , d_data :: Column f ByteString
  , d_headers :: Column f [Header]
  , d_rank :: Column f Double
  , d_title :: Column f Text
  , d_headings :: Column f Text
  , d_content :: Column f Text
  , d_comments :: Column f Text
  , d_stats :: DiscoveryStats f
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Show (Discovery Identity)

data DiscoveryStats f = DiscoveryStats
  { ds_js :: Column f Int32
  , ds_css :: Column f Int32
  , ds_tweets :: Column f Int16
  , ds_gifs :: Column f Int16
  , ds_cookies :: Column f Bool
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Show (DiscoveryStats Identity)
deriving instance Eq (DiscoveryStats Identity)


data Discovery' f = Discovery'
  { d_table :: Discovery f
  , d_search :: Column f Tsvector
  }
  deriving stock Generic
  deriving anyclass Rel8able


data Edges f = Edges
  { e_edgeId :: Column f EdgeId
  , e_src :: Column f DocId
  , e_dst :: Column f DocId
  , e_anchor :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

nextDocId :: Query (Expr DocId)
nextDocId = fmap coerce $ pure $ nextval "doc_id_seq"

{-

CREATE TABLE IF NOT EXISTS assets (
  uri TEXT PRIMARY KEY NOT NULL,
  size int8 NOT NULL
);

-}

assetSchema :: TableSchema (Asset Name)
assetSchema = TableSchema
  { name    = "assets"
  , schema  = Just "public"
  , columns = Asset
      { a_uri   = "uri"
      , a_size = "size"
      }
  }

{-

CREATE SEQUENCE doc_id_seq;
CREATE TABLE IF NOT EXISTS discovery (
  doc_id int8 PRIMARY KEY,
  uri TEXT UNIQUE NOT NULL,
  state VARCHAR(10) NOT NULL,
  depth int4 NOT NULL,
  distance int2[] NOT NULL,
  rank float8 NOT NULL,
  data bytea NOT NULL,
  headers text[] NOT NULL,
  title TEXT NOT NULL,
  headings TEXT NOT NULL,
  content TEXT NOT NULL,
  comments TEXT NOT NULL,

  js int4 NOT NULL,
  css int4 NOT NULL,
  tweets int2 NOT NULL,
  gifs int2 NOT NULL,
  cookies bool NOT NULL
);

ALTER TABLE discovery
    ADD COLUMN distance int2[]
    NOT NULL
    DEFAULT CAST(ARRAY[null,null,null,null,null,null,null,null,null,null] AS int2[]);

ALTER TABLE discovery ADD COLUMN js int4 NULL DEFAULT 0;
ALTER TABLE discovery ADD COLUMN css int4 NULL DEFAULT 0;
ALTER TABLE discovery ADD COLUMN tweets int2 NULL DEFAULT 0;
ALTER TABLE discovery ADD COLUMN gifs int2 NULL DEFAULT 0;
ALTER TABLE discovery ADD COLUMN cookies bool NULL DEFAULT false;
ALTER TABLE discovery ADD COLUMN headers text[] NULL DEFAULT CAST(ARRAY[] AS text[]);

ALTER TABLE discovery
    ADD COLUMN search tsvector
    GENERATED ALWAYS AS (setweight(to_tsvector('english', title), 'A')
               || ' ' || setweight(to_tsvector('english', headings), 'B')
               || ' ' || setweight(to_tsvector('english', content), 'C')
               || ' ' || setweight(to_tsvector('english', comments), 'D')
                ) STORED;

CREATE INDEX search_idx ON discovery USING GIN (search);


-}

discoverySchema :: TableSchema (Discovery Name)
discoverySchema = TableSchema
  { name    = "discovery"
  , schema  = Just "public"
  , columns = Discovery
      { d_docId = "doc_id"
      , d_uri   = "uri"
      , d_state = "state"
      , d_depth = "depth"
      , d_distance = "distance"
      , d_data  = "data"
      , d_headers  = "headers"
      , d_rank = "rank"
      , d_title = "title"
      , d_headings = "headings"
      , d_content = "content"
      , d_comments = "comments"
      , d_stats = discoveryStatsSchema
      }
  }

discoveryStatsSchema :: DiscoveryStats Name
discoveryStatsSchema = DiscoveryStats
  { ds_js = "js"
  , ds_css = "css"
  , ds_tweets = "tweets"
  , ds_gifs = "gifs"
  , ds_cookies = "cookies"
  }

discoverySchema' :: TableSchema (Discovery' Name)
discoverySchema' = discoverySchema
  { columns = Discovery'
      { d_table = columns discoverySchema
      , d_search = "search"
      }
  }


nextEdgeId :: Query (Expr EdgeId)
nextEdgeId = fmap coerce $ pure $ nextval "edge_id_seq"

{-

CREATE SEQUENCE edge_id_seq;
CREATE TABLE IF NOT EXISTS edges (
  id int8 PRIMARY KEY,
  dst int8 NOT NULL REFERENCES discovery(doc_id) ON DELETE CASCADE,
  src int8 NOT NULL REFERENCES discovery(doc_id) ON DELETE CASCADE,
  anchor TEXT NOT NULL
);

CREATE INDEX depth_idx ON discovery (depth);

CREATE INDEX src_idx ON edges (src);
CREATE INDEX dst_idx ON edges (dst);

delete from edges where not exists (select * from discovery as d where d.doc_id = edges.src);
ALTER TABLE edges ADD CONSTRAINT fk_src FOREIGN KEY (src) REFERENCES discovery(doc_id) ON DELETE CASCADE;

delete from edges where not exists (select * from discovery as d where d.doc_id = edges.dst);
ALTER TABLE edges ADD CONSTRAINT fk_dst FOREIGN KEY (dst) REFERENCES discovery(doc_id) ON DELETE CASCADE;


-- CASCADE PRUNE AFTER EDGES ARE DELETED
select from discovery where not exists (select * from edges as e where e.dst = discovery.doc_id) and depth > 0;

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


-}

edgesSchema :: TableSchema (Edges Name)
edgesSchema = TableSchema
  { name    = "edges"
  , schema  = Just "public"
  , columns = Edges
      { e_edgeId = "id"
      , e_src = "src"
      , e_dst = "dst"
      , e_anchor = "anchor"
      }
  }


connectionSettings :: Settings
connectionSettings =
  settings cfg_pg_host cfg_pg_port cfg_pg_user cfg_pg_pass "db"


simpInsert
    :: ( Table Expr (Transpose Expr names)
       , Table Name names
       , Transpose Name (Transpose Expr names) ~ names
       , Columns names ~ Columns (Transpose Expr names)
       )
    => TableSchema names
    -> Query (Transpose Expr names)
    -> Insert ()
simpInsert s e =
  Insert
    { into = s
    , rows = e
    , onConflict = DoNothing
    , returning = pure ()
    }


litInsert
  :: ( Table Name (Transpose Name (Query a))
     , Table Expr a
     , Table Expr (Query a)
     , Foldable f
     , Transpose Expr (Transpose Name (Query a)) ~ Query a
     , Columns (Transpose Name (Query a)) ~ Columns (Query a)
     )
  => TableSchema (Transpose Name (Query a))
  -> f a
  -> Insert ()
litInsert s = simpInsert s . pure . values


rootSites :: [Expr Text]
rootSites =
  -- Due to uri normalization, it's important to not have a trailing slash on
  -- these
  [ "https://slatestarcodex.com"  -- rationality / econ
  , "https://jeremykun.com"       -- math
  , "https://neocities.org"       -- amateur
  , "https://mitxela.com"         -- diy
  , "https://seirdy.one"          -- search engines
  ]

numRootSites :: Int
numRootSites = length rootSites

nullDist :: Expr [Maybe Int16]
nullDist = arrayFill (lit $ fromIntegral numRootSites) null


rootNodes :: Insert ()
rootNodes = Insert
  { into = discoverySchema
  , rows = do
      d <- nextDocId
      (idx, z) <- values $ zip (fmap lit [0..]) rootSites
      pure $ Discovery
        { d_docId = d
        , d_uri = z
        , d_state = lit Discovered
        , d_depth = 0
        , d_data = ""
        , d_headers = lit []
        , d_distance = insertAt' (lit $ fromIntegral numRootSites) idx $ nullify 0
        , d_rank = 0
        , d_title = ""
        , d_headings = ""
        , d_content = ""
        , d_comments = ""
        , d_stats = DiscoveryStats 0 0 0 0 (lit False)
        }
  , onConflict = DoUpdate Upsert
      { index = d_uri
      , set = \new old -> old { d_distance = d_distance new }
      , updateWhere = \new old -> d_uri new ==. d_uri old
      }
  , returning = pure ()
  }

data SearchResult f = SearchResult
  { sr_ranking :: !(Column f Float)
  , sr_id      :: !(Column f DocId)
  , sr_uri     :: !(Column f Text)
  , sr_title   :: !(Column f Text)
  , sr_stats   :: !(DiscoveryStats f)
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Eq (SearchResult Identity)

