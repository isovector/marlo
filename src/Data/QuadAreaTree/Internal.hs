{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE PatternSynonyms     #-}

module Data.QuadAreaTree.Internal where

import Control.Applicative
import Data.Foldable (asum, toList)
import Data.Maybe (isJust, fromMaybe)
import GHC.Generics (Generic)
import Linear.V2


type Region = Quad Int

pattern Region :: Int -> Int -> Int -> Int -> Region
pattern Region { r_x, r_y, r_w, r_h } = Quad r_x r_y r_w r_h
{-# COMPLETE Region #-}


data Quad a
  = Quad !a !a !a !a
  deriving (Show, Read, Eq, Functor, Foldable, Traversable, Generic)

instance Applicative Quad where
  pure a = Quad a a a a
  Quad ftl ftr fbl fbr <*> Quad atl atr abl abr =
    Quad (ftl atl) (ftr atr) (fbl abl) (fbr abr)


subdivide :: Region -> Quad Region
subdivide (Region x y 1 1) =
  let r = Region x y 0 0
   in Quad r r r r
subdivide (Region x y w h) =
  let halfw = div w 2
      halfh = div h 2
   in Quad
        (sanitizeRegion $ Region x y halfw halfh)
        (sanitizeRegion $ Region (x + halfw) y (w - halfw) halfh)
        (sanitizeRegion $ Region x (y + halfh) halfw (h - halfh))
        (sanitizeRegion $ Region (x + halfw) (y + halfh) (w - halfw) (h - halfh))


sanitizeRegion :: Region -> Region
sanitizeRegion r@(Region x y w h)
  | w <= 0 || h <= 0
  = Region x y 0 0
  | otherwise
  = r


containsRegion :: Region -> Region -> Bool
containsRegion r1@(Region bx by bw bh) r2@(Region sx sy sw sh) =
  r1 == r2 ||
  and
    [ bx <= sx
    , by <= sy
    , sx + sw <= bx + bw
    , sy + sh <= by + bh
    ]


containsPoint :: Region -> V2 Int -> Bool
containsPoint (Region x y w h) (V2 tx ty) =
  and
    [ x <= tx
    , y <= ty
    , tx < x + w
    , ty < y + h
    ]


corners :: Region -> [V2 Int]
corners (Region x y w h) = do
  dx <- [0, w - 1]
  dy <- [0, h - 1]
  pure $ V2 (x + dx) (y + dy)


intersects :: Region -> Region -> Bool
intersects r1 r2 = isJust $ getIntersection r1 r2


getIntersection :: Region -> Region -> Maybe Region
getIntersection r1 r2 =
  let x0 = max (r_x r1) (r_x r2)
      y0 = max (r_y r1) (r_y r2)
      x1 = min (r_x r1 + r_w r1) (r_x r2 + r_w r2)
      y1 = min (r_y r1 + r_h r1) (r_y r2 + r_h r2)
      w = x1 - x0
      h = y1 - y0
   in case 0 < w && 0 < h of
        True -> Just $ Region x0 y0 w h
        False -> Nothing


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


insert :: a -> V2 Int -> Quadrant (Region, a) -> Quadrant a
insert v (V2 x y) = fill v (Region x y 1 1)


isEmptyRegion :: Region -> Bool
isEmptyRegion (Quad _ _ x y) = x <= 0 || y <= 0


fill :: a -> Region -> Quadrant (Region, a) -> Quadrant a
fill _ what q | isEmptyRegion what = fmap snd q
fill _ _ q@(Leaf (r, _))
  | isEmptyRegion r
  = fmap snd q
fill v what q@(Leaf (r, a))
  | containsRegion what r
  = pure v
  | intersects what r
  = subFill v what $ fmap (pure . (, a)) $ subdivide r
  | otherwise
  = fmap snd q
fill v what (Node qu) = subFill v what qu


quadSize :: Quadrant Region -> Region
quadSize (Leaf r) = r
quadSize (Node (Quad tl _ _ br)) =
  let Region x y _ _ = quadSize tl
      Region x' y' w h = quadSize br
   in Region x y (x' + w) (y' + h)


subFill :: a -> Region -> Quad (Quadrant (Region, a)) -> Quadrant a
subFill v what q = Node $
  fill v
    <$> fmap (fromMaybe $ Quad 0 0 0 0)
          (getIntersection
              <$> pure what
              <*> fmap (quadSize . fmap fst) q)
    <*> q


getLocation :: V2 Int -> Quadrant (Region, a) -> Maybe a
getLocation p (Leaf (r, a))
  | containsPoint r p = Just a
  | otherwise = Nothing
getLocation p (Node qu) = asum $ getLocation p <$> (toList qu)


apRegion :: Region -> Quadrant a -> Quadrant (Region, a)
apRegion r (Leaf a) = Leaf (r, a)
apRegion r (Node qu)
  = Node $ apRegion <$> subdivide r <*> qu


regionSize :: Region -> Int
regionSize (Quad _ _ w h) = w * h

