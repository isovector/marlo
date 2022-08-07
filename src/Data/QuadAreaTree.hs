{-# LANGUAGE PatternSynonyms #-}

module Data.QuadAreaTree
  (
    -- * Important types
    QuadTree
  , type Region
  , pattern I.Region
  , V2 (..)

    -- * Construction
  , makeTree
  , insert
  , fill

    -- * Destruction
  , foldTree
  , tightlySatisfying
  , hitTest
  , pointMap
  , getLocation
  , asWeighted
  , tile

    -- * Debugging
  , showTree

    -- * Helpers
  , bounds
  , inBounds

    -- * Geometry
  , Quad (..)
  , subdivide
  , containsRegion
  , containsPoint
  , intersects
  , getIntersection
  , regionSize
  , regionPoints
  , corners
  ) where

import           Control.Arrow (first)
import           Data.Bool (bool)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.QuadAreaTree.Geometry
import           Data.QuadAreaTree.Internal (Quadrant, Squadrant)
import qualified Data.QuadAreaTree.Internal as I
import           GHC.Generics (Generic)
import           Linear.V2


data QuadTree a = Wrapper
  { qt_quad :: Quadrant a
  , qt_size :: Region
  }
  deriving (Show, Read, Eq, Functor, Generic, Traversable)

instance Foldable QuadTree where
  foldMap = foldTree . const


foldTree :: Monoid m => (Region -> a -> m) -> QuadTree a -> m
foldTree f = I.foldTree f . regionify


fill :: a -> Region -> QuadTree a -> QuadTree a
fill a r = liftTree $ I.fill a r


insert :: a -> V2 Int -> QuadTree a -> QuadTree a
insert a r = liftTree $ I.insert a r


getLocation :: QuadTree a -> V2 Int -> Maybe a
getLocation q v =  I.getLocation v $ regionify q


regionify :: QuadTree a -> Squadrant a
regionify (Wrapper q r) = (r, q)


showTree :: (a -> Char) -> QuadTree a -> String
showTree f q@(Wrapper _ (I.Region x y w h)) = do
  let pm = pointMap q
  unlines $ do
    yp <- [y .. y + h - 1]
    pure $ do
      xp <- [x .. x + w - 1]
      let p = V2 xp yp
      case M.lookup p pm of
        Nothing -> error $ "indexed a bad spot! " <> show p
        Just a -> pure $ f a


asWeighted :: Show a => QuadTree a -> [a]
asWeighted = (uncurry replicate . first I.regionSize  =<<) . tile


makeTree :: Region -> a -> QuadTree a
makeTree r a = Wrapper (I.Leaf a) r


liftTree :: (Squadrant a -> Quadrant a) -> QuadTree a -> QuadTree a
liftTree f w = w { qt_quad = f $ regionify w }


tile :: QuadTree a -> [(Region, a)]
tile = I.tile . regionify


pointMap :: QuadTree a -> Map (V2 Int) a
pointMap
  = foldTree (\r a -> M.fromList $ fmap (, a) $ regionPoints r)


hitTest :: Monoid m => (a -> m) -> Region -> QuadTree a -> m
hitTest f r = I.hitTest (const f) r . regionify


bounds :: QuadTree a -> Region
bounds = qt_size


inBounds :: QuadTree a -> Region -> Bool
inBounds = containsRegion . bounds


tightlySatisfying :: (a -> Bool) -> QuadTree a -> Region
tightlySatisfying f =
  foldTree $ \r a -> bool mempty r $ f a

