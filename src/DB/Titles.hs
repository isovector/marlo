{-

CREATE SEQUENCE title_seg_id_seq;
CREATE TABLE IF NOT EXISTS title_segs (
  id int8 PRIMARY KEY,
  seg text UNIQUE NOT NULL
);

CREATE SEQUENCE title_edge_id_seq;
CREATE TABLE IF NOT EXISTS title_edges (
  id int8 PRIMARY KEY,
  doc int8 NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  seg int8 NOT NULL REFERENCES title_segs(id) ON DELETE CASCADE
);

CREATE INDEX title_edge_doc ON title_edges (doc);
CREATE INDEX title_edge_seg ON title_edges (seg);

-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DB.Titles where

import Data.Coerce (coerce)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Types
import Rel8.Machinery


data TitleSeg f = TitleSeg
  { ts_id  :: Column f TitleSegId
  , ts_seg :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

instance HasUniqueId TitleSeg TitleSegId where
  allRows = titleSegSchema
  uniqueId = ts_id
  nextUniqueId = nextTitleSegId


titleSegSchema :: TableSchema (TitleSeg Name)
titleSegSchema = TableSchema
  { name    = "title_segs"
  , schema  = Just "public"
  , columns = TitleSeg
      { ts_id = "id"
      , ts_seg = "seg"
      }
  }

nextTitleSegId :: Query (Expr TitleSegId)
nextTitleSegId = fmap coerce $ pure $ nextval "title_seg_id_seq"


data TitleEdge f = TitleEdge
  { te_id  :: Column f TitleEdgeId
  , te_doc :: Column f DocId
  , te_seg :: Column f TitleSegId
  }
  deriving stock Generic
  deriving anyclass Rel8able

instance HasUniqueId TitleEdge TitleEdgeId where
  allRows = titleEdgeSchema
  uniqueId = te_id
  nextUniqueId = nextTitleEdgeId


titleEdgeSchema :: TableSchema (TitleEdge Name)
titleEdgeSchema = TableSchema
  { name    = "title_edges"
  , schema  = Just "public"
  , columns = TitleEdge
      { te_id = "id"
      , te_doc = "doc"
      , te_seg = "seg"
      }
  }

nextTitleEdgeId :: Query (Expr TitleEdgeId)
nextTitleEdgeId = fmap coerce $ pure $ nextval "title_edge_id_seq"

