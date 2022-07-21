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

import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Int (Int64, Int16, Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Connection (Settings, settings)
import Rel8 hiding (Enum)
import Types
import Data.ByteString (ByteString)


newtype EdgeId = EdgeId
  { unEdgeId :: Int64
  }
  deriving newtype (Show, DBType, DBEq, DBOrd)

newtype DocId = DocId
  { unDocId :: Int64
  }
  deriving newtype (Show, DBType, DBEq, DBOrd)

newtype WordId = WordId
  { unWordId :: Int64
  }
  deriving newtype (Show, DBType, DBEq, DBOrd)

newtype IndexId = IndexId
  { unIndexId :: Int64
  }
  deriving newtype (Show, DBType, DBEq, DBOrd)

data DiscoveryState
  = Discovered
  | Explored
  | Pruned
  | Errored
  | Unacceptable
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (DBType, DBEq) via ReadShow DiscoveryState

data Discovery f = Discovery
  { d_docId :: Column f DocId
  , d_uri   :: Column f Text
  , d_state :: Column f DiscoveryState
  , d_depth :: Column f Int32
  , d_data :: Column f ByteString
  , d_rank :: Column f Double
  , d_title :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Show (Discovery Identity)

data Words f = Words
  { w_wordId :: Column f WordId
  , w_word :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Show (Words Identity)

data Index f = Index
  { i_id :: Column f IndexId
  , i_docId :: Column f DocId
  , i_wordId :: Column f WordId
  , i_position :: Column f Int16
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

data InverseIndex f = InverseIndex
  { ii_wordId :: Column f WordId
  , ii_docs :: Column f [DocId]
  }
  deriving stock Generic
  deriving anyclass Rel8able

nextDocId :: Query (Expr DocId)
nextDocId = fmap coerce $ pure $ nextval "doc_id_seq"

{-

CREATE SEQUENCE doc_id_seq;
CREATE TABLE IF NOT EXISTS discovery (
  doc_id int8 PRIMARY KEY,
  uri TEXT UNIQUE NOT NULL,
  state VARCHAR(10) NOT NULL,
  depth int4 NOT NULL,
  rank float8 NOT NULL,
  data bytea NOT NULL,
  title TEXT UNIQUE NOT NULL
);

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
      , d_data  = "data"
      , d_rank = "rank"
      , d_title = "title"
      }
  }

nextWordId :: Query (Expr WordId)
nextWordId = fmap coerce $ pure $ nextval "word_id_seq"

{-

CREATE SEQUENCE word_id_seq;
CREATE TABLE IF NOT EXISTS words (
  id int8 PRIMARY KEY,
  word TEXT UNIQUE NOT NULL
);

-}


wordsSchema :: TableSchema (Words Name)
wordsSchema = TableSchema
  { name    = "words"
  , schema  = Just "public"
  , columns = Words
      { w_wordId = "id"
      , w_word = "word"
      }
  }

nextEdgeId :: Query (Expr EdgeId)
nextEdgeId = fmap coerce $ pure $ nextval "edge_id_seq"

{-

CREATE SEQUENCE edge_id_seq;
CREATE TABLE IF NOT EXISTS edges (
  id int8 PRIMARY KEY,
  src TEXT NOT NULL,
  dst TEXT NOT NULL,
  anchor TEXT NOT NULL
);

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

nextIndexId :: Query (Expr IndexId)
nextIndexId = fmap coerce $ pure $ nextval "index_id_seq"

{-

CREATE SEQUENCE index_id_seq;
CREATE TABLE IF NOT EXISTS index (
  id int8 PRIMARY KEY,
  doc_id int8 NOT NULL,
  word_id int8 NOT NULL,
  position int4 NOT NULL
);

-}

indexSchema :: TableSchema (Index Name)
indexSchema = TableSchema
  { name    = "index"
  , schema  = Just "public"
  , columns = Index
      { i_id = "id"
      , i_docId = "doc_id"
      , i_wordId = "word_id"
      , i_position = "position"
      }
  }

-- (<@.) :: DBEq a => Expr [a] -> Expr [a] -> Expr Bool
-- a <@. b = unsafeLiftOpNull _ a b

{-

CREATE TABLE IF NOT EXISTS inverse_index (
  word_id int8 PRIMARY KEY,
  documents int8[] NOT NULL
);

-}

inverseIndexSchema :: TableSchema (InverseIndex Name)
inverseIndexSchema = TableSchema
  { name    = "inverse_index"
  , schema  = Just "public"
  , columns = InverseIndex
      { ii_wordId = "word_id"
      , ii_docs = "documents"
      }
  }



connectionSettings :: Settings
connectionSettings = settings "localhost" 5432 "postgres" "" "db"



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


rootNodes :: Insert ()
rootNodes = Insert
  { into = discoverySchema
  , rows = do
      d <- nextDocId
      z <- values
            [ "https://astralcodexten.substack.com"
            , "https://blog.plover.com/"
            , "https://overcomingbias.com/"
            , "https://marginalrevolution.com/"
            , "https://lesswrong.com/"
            , "http://www.paulgraham.com/articles.html"
            , "https://apxhard.substack.com/"
            , "https://what-if.xkcd.com/"
            , "https://jeremykun.com/"
            , "https://sandymaguire.me/"
            ]
      pure Discovery
        { d_docId = d
        , d_uri = z
        , d_state = lit Discovered
        , d_depth = 0
        , d_data = ""
        , d_rank = 0
        , d_title = ""
        }
  , onConflict = DoNothing
  , returning = pure ()
  }

