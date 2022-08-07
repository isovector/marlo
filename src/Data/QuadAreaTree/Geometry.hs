{-# LANGUAGE PatternSynonyms     #-}

module Data.QuadAreaTree.Geometry where

import GHC.Generics (Generic)
import Linear.V2
import Data.Maybe (isJust)
import Data.Monoid

data Quad a
  = Quad !a !a !a !a
  deriving stock (Show, Read, Eq, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via (Ap Quad a)

instance Applicative Quad where
  pure a = Quad a a a a
  Quad ftl ftr fbl fbr <*> Quad atl atr abl abr =
    Quad (ftl atl) (ftr atr) (fbl abl) (fbr abr)


type Region = Quad Int

pattern Region :: Int -> Int -> Int -> Int -> Region
pattern Region { r_x, r_y, r_w, r_h } = Quad r_x r_y r_w r_h
{-# COMPLETE Region #-}

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
containsPoint (Region _ _ w h) _
  | w <= 0 || h <= 0
  = False
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


regionSize :: Region -> Int
regionSize (Quad _ _ w h) = w * h

