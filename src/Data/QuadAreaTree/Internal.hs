module Data.QuadAreaTree.Internal
  ( module Data.QuadAreaTree.Internal
  , Region (..)
  , Quad (..)
  , regionSize
  ) where

import Data.Foldable (fold)
import Data.Monoid
import Data.QuadAreaTree.Geometry
import Linear.V2
import Control.Applicative (liftA2)
import GHC.Generics (Generic)


type Squadrant a = (Region, Quadrant a)


data Quadrant a
  = Leaf a
  | Node (Quad (Quadrant a))
  deriving stock (Show, Read, Eq, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Quadrant a

instance Applicative Quadrant where
  pure = Leaf
  Leaf f <*> a = fmap (f $) a
  f <*> Leaf a = fmap ($ a) f
  Node f <*> Node a = Node $ liftA2 (<*>) f a


-- TODO(sandy): doesn't satisfy the laws. not sure why, but I don't need this.

-- instance Monad Quadrant where
--   Leaf a >>= f = f a
--   Node (Quad tl tr bl br) >>= f =
--     Node $ Quad (tl >>= f) (tr >>= f) (bl >>= f) (br >>= f)


insert :: Monoid a => a -> V2 Int -> Squadrant a -> Quadrant a
insert v (V2 x y) = fill v (Region x y 1 1)


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


foldTree
    :: forall m a
     . Monoid m
    => (Region -> a -> m)
    -> Squadrant a
    -> m
foldTree _ (r, Leaf _)
  | isEmptyRegion r = mempty
foldTree f (r, Leaf a)
  = f r a
foldTree f (r, Node q) =
  foldMap (foldTree f) $
    (,) <$> subdivide r <*> q


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


fill :: Monoid a => a -> Region -> Squadrant a -> Quadrant a
fill _ what q | isEmptyRegion what = snd q
fill _ _ q@(r, Leaf{})
  | isEmptyRegion r
  = snd q
fill v what q@(r, Leaf a)
  | containsRegion what r
  = Leaf (v <> a)
  | intersects what r
  = subFill v what (r, splitLeaf a)
  | otherwise
  = snd q
fill v what (r, Node qu) = subFill v what (r, qu)


tile :: Squadrant a -> [(Region, a)]
tile q = foldTree ((pure .) . (,)) q


subFill :: Monoid a => a -> Region -> (Region, Quad (Quadrant a)) -> Quadrant a
subFill v what q = Node $ origami id (fill v) what q


getLocation :: Monoid a => V2 Int -> Squadrant a -> a
getLocation (V2 x y) = hitTest (const id) (Region x y 1 1)

