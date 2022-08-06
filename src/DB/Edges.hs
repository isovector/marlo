{-

CREATE SEQUENCE edge_id_seq;
CREATE TABLE IF NOT EXISTS edges (
  id int8 PRIMARY KEY,
  dst int8 NOT NULL REFERENCES discovery(doc_id) ON DELETE CASCADE,
  src int8 NOT NULL REFERENCES discovery(doc_id) ON DELETE CASCADE,
  anchor TEXT NOT NULL
);

CREATE INDEX src_idx ON edges (src);
CREATE INDEX dst_idx ON edges (dst);

delete from edges where not exists (select * from discovery as d where d.doc_id = edges.src);
ALTER TABLE edges ADD CONSTRAINT fk_src FOREIGN KEY (src) REFERENCES discovery(doc_id) ON DELETE CASCADE;

delete from edges where not exists (select * from discovery as d where d.doc_id = edges.dst);
ALTER TABLE edges ADD CONSTRAINT fk_dst FOREIGN KEY (dst) REFERENCES discovery(doc_id) ON DELETE CASCADE;


-- CASCADE PRUNE AFTER EDGES ARE DELETED
select from discovery where not exists (select * from edges as e where e.dst = discovery.doc_id) and depth > 0;

-}

module DB.Edges where

import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Types


data Edges f = Edges
  { e_edgeId :: Column f EdgeId
  , e_src    :: Column f DocId
  , e_dst    :: Column f DocId
  , e_anchor :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Eq (Edges Identity)
deriving instance Show (Edges Identity)


nextEdgeId :: Query (Expr EdgeId)
nextEdgeId = fmap coerce $ pure $ nextval "edge_id_seq"


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

