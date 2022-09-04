{-

CREATE TABLE IF NOT EXISTS edges (
  src int8 NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  dst int8 NOT NULL REFERENCES discovery(id) ON DELETE CASCADE,
  PRIMARY KEY (src, dst)
);


ALTER TABLE edges DROP COLUMN id;
ALTER TABLE edges DROP COLUMN anchor;

DELETE FROM edges
WHERE ctid NOT IN (
  SELECT MIN(ctid)
  FROM edges
  GROUP BY src, dst
);

DELETE FROM edges a USING (
      SELECT MIN(ctid) as ctid, src, dst
        FROM edges
        GROUP BY src, dst HAVING COUNT(*) > 1
      ) b
      WHERE a.src = b.src
      AND a.dst = b.dst
      AND a.ctid <> b.ctid;

ALTER TABLE edges ADD PRIMARY KEY (src, dst);


CREATE INDEX src_idx ON edges (src);
CREATE INDEX dst_idx ON edges (dst);

-}

module DB.Edges where

import Data.Functor.Identity
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Types


data Edges f = Edges
  { e_src    :: Column f DocId
  , e_dst    :: Column f DiscId
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Eq (Edges Identity)
deriving instance Show (Edges Identity)


edgesSchema :: TableSchema (Edges Name)
edgesSchema = TableSchema
  { name    = "edges"
  , schema  = Just "public"
  , columns = Edges
      { e_src = "src"
      , e_dst = "dst"
      }
  }

