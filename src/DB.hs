{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DB where

import           Data.Coerce (coerce)
import           Data.Int (Int64)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Rel8
import           Rel8 hiding (Enum)
import           Types

newtype EdgeId = EdgeId
  { getEdgeId :: Int64
  }
  deriving newtype (DBType, DBEq)

newtype DocId = DocId
  { getDocId :: Int64
  }
  deriving newtype (DBType, DBEq)

newtype WordId = WordId
  { getWordId :: Int64
  }
  deriving newtype (DBType, DBEq)

data DiscoveryState
  = Discovered
  | Explored
  | Pruned
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving DBType via ReadShow DiscoveryState

data Discovery f = Discovery
  { d_docId :: Column f DocId
  , d_uri   :: Column f Text
  , d_state :: Column f DiscoveryState
  }
  deriving stock Generic
  deriving anyclass Rel8able

data Words f = Words
  { w_wordId :: Column f WordId
  , w_word :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

data Index f = Index
  { i_docId :: Column f DocId
  , i_wordId :: Column f WordId
  , i_positions :: Column f [Int64]
  }
  deriving stock Generic
  deriving anyclass Rel8able

data Edges f = Edges
  { e_edgeId :: Column f EdgeId
  , e_src :: Column f DocId
  , e_dst :: Column f DocId
  }
  deriving stock Generic
  deriving anyclass Rel8able

data InverseIndex f = InverseIndex
  { ii_wordId :: Column f WordId
  , ii_docs :: Column f [DocId]
  }
  deriving stock Generic
  deriving anyclass Rel8able

discoverySchema :: TableSchema (Discovery Name)
discoverySchema = TableSchema
  { name    = "discovery"
  , schema  = Just "db"
  , columns = Discovery
      { d_docId = "doc_id"
      , d_uri   = "uri"
      , d_state = "state"
      }
  }

wordsSchema :: TableSchema (Words Name)
wordsSchema = TableSchema
  { name    = "discovery"
  , schema  = Just "db"
  , columns = Words
      { w_wordId = "word_id"
      , w_word = "word"
      }
  }

edgesSchema :: TableSchema (Edges Name)
edgesSchema = TableSchema
  { name    = "discovery"
  , schema  = Just "db"
  , columns = Edges
      { e_edgeId = "id"
      , e_src = "src"
      , e_dst = "dst"
      }
  }

indexSchema :: TableSchema (Index Name)
indexSchema = TableSchema
  { name    = "discovery"
  , schema  = Just "db"
  , columns = Index
      { i_docId = "doc_id"
      , i_wordId = "word_id"
      , i_positions = "positions"
      }
  }

-- (<@.) :: DBEq a => Expr [a] -> Expr [a] -> Expr Bool
-- a <@. b = unsafeLiftOpNull _ a b

inverseIndexSchema :: TableSchema (InverseIndex Name)
inverseIndexSchema = TableSchema
  { name    = "discovery"
  , schema  = Just "db"
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





