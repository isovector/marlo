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

import           Data.Coerce (coerce)
import           Data.Int (Int64, Int16)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Rel8
import           Rel8 hiding (Enum)
import           Types
import Hasql.Connection (Settings, settings, acquire)
import Hasql.Session (run, statement)
import Data.Functor.Identity

newtype EdgeId = EdgeId
  { getEdgeId :: Int64
  }
  deriving newtype (Show, DBType, DBEq)

newtype DocId = DocId
  { getDocId :: Int64
  }
  deriving newtype (Show, DBType, DBEq)

newtype WordId = WordId
  { getWordId :: Int64
  }
  deriving newtype (Show, DBType, DBEq)

data DiscoveryState
  = Discovered
  | Explored
  | Pruned
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (DBType, DBEq) via ReadShow DiscoveryState

data Discovery f = Discovery
  { d_docId :: Column f DocId
  , d_uri   :: Column f Text
  , d_state :: Column f DiscoveryState
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

data Index f = Index
  { i_docId :: Column f DocId
  , i_wordId :: Column f WordId
  , i_positions :: Column f [Int16]
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

{-

CREATE TABLE IF NOT EXISTS discovery (
  doc_id int8 PRIMARY KEY,
  uri TEXT UNIQUE NOT NULL,
  state VARCHAR(10) NOT NULL
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
      }
  }

{-

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

{-

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

{-

CREATE TABLE IF NOT EXISTS index (
  doc_id int8 NOT NULL,
  word_id int8 NOT NULL,
  positions int4[] NOT NULL,
  PRIMARY KEY(doc_id, word_id)
);

-}

indexSchema :: TableSchema (Index Name)
indexSchema = TableSchema
  { name    = "index"
  , schema  = Just "public"
  , columns = Index
      { i_docId = "doc_id"
      , i_wordId = "word_id"
      , i_positions = "positions"
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


-- discovery :: URI -> [Link] ->



test :: [Keyword] -> Query (Expr [DocId])
test kws' = do
  ws  <- each wordsSchema
  kws <- values $ fmap lit $ coerce @_ @[Text] kws'
  where_ $ w_word ws ==. kws
  ii <- each inverseIndexSchema
  where_ $ ii_wordId ii ==. w_wordId ws
  pure $ ii_docs ii
  -- z <- aggregate $ fmap listAgg $ pure $ ii_docs ii
  -- _

  -- -- where_ $ pure $ w_word ws `elem` kws
  -- pure [ws]

--   where_ $ ws

connectionSettings :: Settings
connectionSettings = settings "localhost" 5432 "postgres" "" "db"


nextDiscovered :: Query (Discovery Expr)
nextDiscovered = limit 1 $ do
  d <- each discoverySchema
  where_ $ d_state d ==. lit Discovered
  pure d


markExplored :: DiscoveryState -> Discovery Identity -> Update ()
markExplored ds d = Update
  { target = discoverySchema
  , from = pure ()
  , set = \ _ dis -> dis { d_state = lit ds }
  , updateWhere = \ _ dis -> d_docId dis ==. lit (d_docId d)
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
litInsert s e =
  Insert
    { into = s
    , rows = pure $ values e
    , onConflict = DoNothing
    , returning = pure ()
    }








-- main :: IO Int64
main = do
  Right connect <- acquire connectionSettings
  flip run connect $ statement () $ select nextDiscovered
  -- flip run connect $ statement () $ update $ markExplored Explored res
    -- Insert
    --   { into = discoverySchema
    --   , rows = pure $ lit $ Discovery (DocId 1) "http://yo.com/yo" Discovered
    --   , onConflict = DoNothing
    --   , returning = NumberOfRowsAffected
    --   }




