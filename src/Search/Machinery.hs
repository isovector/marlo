{-# LANGUAGE AllowAmbiguousTypes #-}

module Search.Machinery where

import Types
import Rel8
import Data.Kind (Type, Constraint)
import DB
import Data.Text (Text)
import Data.Int (Int64)
import Lucid (Html)


data SSearchVariety (v :: SearchVariety) where
  STraditional :: SSearchVariety 'Traditional
  SSpatial     :: SSearchVariety 'Spatial


data SomeSearchVariety where
  SomeSearchVariety
      :: SSearchVariety v
      -> SomeSearchVariety


data Dict1 (c :: SearchVariety -> Constraint) (v :: SearchVariety) where
  Dict1 :: c v => Dict1 c v

dict1 :: (c 'Traditional, c 'Spatial) => SSearchVariety v -> Dict1 c v
dict1 STraditional = Dict1
dict1 SSpatial = Dict1


toSing :: SearchVariety -> SomeSearchVariety
toSing Traditional = SomeSearchVariety STraditional
toSing Spatial = SomeSearchVariety SSpatial


fromSomeSing :: SomeSearchVariety -> SearchVariety
fromSomeSing (SomeSearchVariety STraditional) = Traditional
fromSomeSing (SomeSearchVariety SSpatial) = Spatial


fromSing :: forall v. SSearchVariety v -> SearchVariety
fromSing = fromSomeSing . SomeSearchVariety


class Demote (v :: SearchVariety) where
  demote :: SearchVariety

instance Demote 'Traditional where
  demote = Traditional

instance Demote 'Spatial where
  demote = Spatial


class Demote v => SearchMethod (v :: SearchVariety) where
  type SearchMethodResult v :: Type
  prepareSearch
      :: Word
      -> Query (Expr Int64, SearchResult Expr)
      -> Query (Expr Int64, SearchResult Expr)
  accumResults
      :: Connection
      -> Search Text
      -> [SearchResult Identity]
      -> IO [SearchMethodResult v]
  showResults
      :: Search Text
      -> Int64
      -> Word
      -> [SearchMethodResult v] -> Html ()

