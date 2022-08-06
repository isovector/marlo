{-# LANGUAGE PatternSynonyms #-}

module Data.QuadAreaTree
  ( QuadTree
  , type Region
  , pattern I.Region
  , V2  (..)
  , makeTree
  , fill
  , insert
  , getLocation
  , showTree
  , asWeighted
  , tile
  ) where

import           Control.Arrow (first)
import           Control.Monad (join)
import           Data.Foldable (toList)
import           Data.QuadAreaTree.Internal (Region, Quadrant)
import qualified Data.QuadAreaTree.Internal as I
import           GHC.Generics (Generic)
import           Linear.V2



data QuadTree a = Wrapper
  { qt_quad :: Quadrant a
  , qt_size :: Region
  }
  deriving (Show, Read, Eq, Functor, Generic)


fill :: a -> Region -> QuadTree a -> QuadTree a
fill a r = liftTree $ I.fill a r

insert :: a -> V2 Int -> QuadTree a -> QuadTree a
insert a r = liftTree $ I.insert a r

getLocation :: QuadTree a -> V2 Int -> Maybe a
getLocation q v =  I.getLocation v $ regionify q


regionify :: QuadTree a -> Quadrant (Region, a)
regionify (Wrapper q r) = I.apRegion r q


showTree :: (a -> Char) -> QuadTree a -> String
showTree f q@(Wrapper _ (I.Region x y w h)) =
  unlines $ do
    yp <- [y .. y + h - 1]
    pure $ do
      xp <- [x .. x + w - 1]
      let p = V2 xp yp
      case getLocation q p of
        Nothing -> error $ "indexed a bad spot! " <> show p
        Just a -> pure $ f a



asWeighted :: Show a => QuadTree a -> [a]
asWeighted =  join . fmap (uncurry replicate . first I.regionSize) . toList . regionify


makeTree :: Region -> a -> QuadTree a
makeTree r a = Wrapper (pure a) r


liftTree :: (Quadrant (Region, a) -> Quadrant a) -> QuadTree a -> QuadTree a
liftTree f w = w { qt_quad = f $ regionify w }


tile :: QuadTree a -> [(Region, a)]
tile = toList . regionify

