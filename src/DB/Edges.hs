{-

CREATE TABLE IF NOT EXISTS assets (
  uri TEXT PRIMARY KEY NOT NULL,
  size int8 NOT NULL
);

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

