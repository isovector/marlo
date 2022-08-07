{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE PatternSynonyms     #-}

module Data.QuadAreaTree.Internal
  ( module Data.QuadAreaTree.Internal
  , pattern Region
  , Region
  , Quad (..)
  , regionSize
  ) where

import Data.Foldable (fold)
import Data.Monoid
import Data.QuadAreaTree.Geometry
import Linear.V2
import Control.Applicative (liftA2)



type Squadrant a = (Region, Quadrant a)


data Quadrant a
  = Leaf a
  | Node (Quad (Quadrant a))
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance Applicative Quadrant where
  pure = Leaf
  Leaf f <*> a = fmap (f $) a
  f <*> Leaf a = fmap ($ a) f
  Node f <*> Node a = Node $ liftA2 (<*>) f a

instance Monad Quadrant where
  Leaf a >>= f = f a
  Node (Quad tl tr bl br) >>= f =
    Node $ Quad (tl >>= f) (tr >>= f) (bl >>= f) (br >>= f)


insert :: a -> V2 Int -> Squadrant a -> Quadrant a
insert v (V2 x y) = fill v (Region x y 1 1)


isEmptyRegion :: Region -> Bool
isEmptyRegion (Quad _ _ x y) = x <= 0 || y <= 0


hitTest
    :: forall m a
     . Monoid m
    => (Region -> a -> m)
    -> Region
    -> Squadrant a
    -> m
hitTest _ what _
  | isEmptyRegion what = mempty
hitTest _ _ (r, Leaf _)
  | isEmptyRegion r = mempty
hitTest f what (r, Leaf a)
  | intersects what r
  = f r a
  | otherwise
  = mempty
hitTest f what (r, Node q) =
  fold $ origami (const mempty) (hitTest f) what (r, q)


origami
    :: (a -> b)                      -- ^ What to do if there is no intersection
    -> (Region -> (Region, a) -> b)  -- ^ What to do on an intersection
    -> Region                        -- ^ Looking for what
    -> (Region, Quad a)              -- ^ In the unnested quad
    -> Quad b
origami miss hit what (r, q) =
  let subr = subdivide r
      subw = fmap (getIntersection what) subr
      -- sel :: Maybe Region -> Region -> Quadrant a -> m
      sel Nothing _ q'   = miss q'
      sel (Just w) r' q' = hit w (r',  q')
   in sel <$> subw <*> subr <*> q


splitLeaf :: a -> Quad (Quadrant a)
splitLeaf a = let l = Leaf a in Quad l l l l


fill :: a -> Region -> Squadrant a -> Quadrant a
fill _ what q | isEmptyRegion what = snd q
fill _ _ q@(r, Leaf{})
  | isEmptyRegion r
  = snd q
fill v what q@(r, Leaf a)
  | containsRegion what r
  = Leaf v
  | intersects what r
  = subFill v what (r, splitLeaf a)
  | otherwise
  = snd q
fill v what (r, Node qu) = subFill v what (r, qu)


tile :: Squadrant a -> [(Region, a)]
tile q@(r, _) = hitTest ((pure .) . (,)) r q


subFill :: forall a. a -> Region -> (Region, Quad (Quadrant a)) -> Quadrant a
subFill v what q = Node $ origami id (fill v) what q


getLocation :: V2 Int -> Squadrant a -> Maybe a
getLocation (V2 x y) = getFirst . hitTest (const pure) (Region x y 1 1)

