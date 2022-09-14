{-# LANGUAGE AllowAmbiguousTypes #-}

module Search.Machinery where

import DB
import Data.Kind (Type, Constraint)
import Data.Text (Text)
import Lucid (Html)
import Types
import Servant.StreamingUtil (Streaming)
import Linear (V3)


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
  limitStrategy :: LimitStrategy
  accumResults
      :: Connection
      -> WindowSize
      -> V3 SearchDimension
      -> Search Text
      -> [SearchResult Identity]
      -> IO (SearchMethodResult v)
  showResults
      :: Connection
      -> Search Text
      -> SearchMethodResult v
      -> Streaming (Html ()) IO ()
  debugResults
     :: SearchMethodResult v -> IO ()

