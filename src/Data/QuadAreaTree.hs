{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.QuadAreaTree where

import Linear.V2
import Control.Applicative
import Data.Foldable (asum, toList, traverse_)
import Text.Show.Pretty (pPrint)
import Test.QuickCheck
import Data.Bool (bool)
import GHC.Generics (Generic)
import Control.Monad (guard, join)
import Control.Arrow (first)
import Debug.Trace (traceShowId, trace)
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy

-- trace = flip const

data QuadTree a = Wrapper
  { wrappedTree :: Quadrant a
    -- | Must be a power of two!!
  , quadSize :: Region
  }
  deriving (Show, Read, Eq, Functor, Generic)


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

getNatVal :: forall a. KnownNat a => Int
getNatVal = fromInteger $ natVal $ Proxy @a


validRegion :: forall w h. (KnownNat w, KnownNat h) => RegionAtLeast w h -> Bool
validRegion (RegionAtLeast (Region _ _ w h)) =
  w >= getNatVal @w && h >= getNatVal @h

newtype RegionAtLeast (w :: Nat) (h :: Nat) = RegionAtLeast
  { getRegionAtLeast :: Region
  }
  deriving stock (Eq, Show, Generic)

instance (KnownNat w, KnownNat h) => Arbitrary (RegionAtLeast w h) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    Positive w <- arbitrary
    Positive h <- arbitrary
    pure $ RegionAtLeast $ Region x y (w + getNatVal @w) (h + getNatVal @h)
  shrink
    = filter validRegion
    . fmap (RegionAtLeast @w @h)
    . genericShrink
    . getRegionAtLeast

deriving via (RegionAtLeast 1 1) instance (Arbitrary Region)


prop_gen_subdivide :: Testable prop => (Region -> Quad Region -> prop) -> Property
prop_gen_subdivide f = property $ do
  r <- arbitrary
  let q = subdivide r
  pure
    $ counterexample (show r)
    $ counterexample (show q)
    $ f r q


props :: IO ()
props = do
  traverse_ quickCheck
    [ prop_gen_subdivide $
        \(Region x y _ _) (Quad (Region x' y' _ _) _ _ _) ->
          x == x' && y == y'

    , prop_gen_subdivide $
        \(Region _ _ w h) (Quad (Region _ _ w1 h1) _ _ (Region _ _ w2 h2)) ->
          w == w1 + w2 && h == h1 + h2

    , prop_gen_subdivide $
        \_ (Quad (Region tlx tly _ _)
                 (Region trx try _ _)
                 (Region blx bly _ _)
                 (Region brx bry _ _)) ->
          and
            [ tlx < trx
            , blx < brx
            , tly < bly
            , try < bry
            , tly == try
            , bly == bry
            , tlx == blx
            , trx == brx
            ]

    , property $ \r -> containsRegion r r

    , property $ \r -> intersects r r

    , property $ \r1 r2 -> not (containsRegion r1 r2) || intersects r1 r2

    , property $ \r1 r2 -> intersects r1 r2 == intersects r2 r1

    , property $ \r ->
        let Quad tl tr bl br = subdivide r
         in and $ do
              r1 <- [tl, tr, bl, br]
              r2 <- [tl, tr, bl, br]
              guard $ r1 /= r2
              pure $ not $ intersects r1 r2

    , property $ \r@(Region x y w h) -> do
        x' <- elements [0 .. w - 2]
        y' <- elements [0 .. h - 2]
        let tree = makeTree r False
        pure $
          length (filter id $ asWeighted $ liftTree (insert True $ V2 (x + x') (y + y')) tree) == 1

    , property $ \(RegionAtLeast (Region x y ((+ 1) -> w) ((+ 1) -> h)) :: RegionAtLeast 2 2) -> do
        x' <- elements [0 .. w - 3]
        y' <- elements [0 .. h - 3]
        let tree = makeTree (Region x y w h) False
            res = liftTree (fill True $ Region (x + x') (y + y') 2 2) tree

        pure $
          counterexample (show x') $
          counterexample (show y') $
          counterexample (show tree) $
          counterexample (show $ length (filter id $ asWeighted res)) $
          length (filter id $ asWeighted res) == 4


    ]


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
intersects r1 r2 = or
  [ any (containsPoint r1) (corners r2)
  , any (containsPoint r2) (corners r1)
  , containsRegion r1 r2
  , containsRegion r2 r1
  ]


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


insert :: Show a => a -> V2 Int -> Quadrant (Region, a) -> Quadrant a
insert v (V2 x y) = fill v (Region x y 1 1)

isEmptyRegion :: Region -> Bool
isEmptyRegion (Quad _ _ x y) = x <= 0 || y <= 0

fill :: Show a => a -> Region -> Quadrant (Region, a) -> Quadrant a
fill _ what q
  | isEmptyRegion what
  = fmap snd q
fill _ _ q@(Leaf (r, _))
  | isEmptyRegion r
  = fmap snd q
fill v what (Leaf (r, a)) =
  if
      -- The entire space is covered by what
    | containsRegion what r -> -- trace "done" $
        Leaf v
      -- The space entirely contains what
    | containsRegion r what -> -- trace ("splits a node: " <> show (what, r)) $
        Node $
          fill v
            <$> pure what
            <*> fmap (pure . (, a)) (subdivide r)
    | intersects what r -> -- trace ("recursing: " <> show (what, r)) $
        Node $
          fill v
            <$> (subdivide what)
            <*> pure (pure (r, a))
    | otherwise -> -- trace ("no interaction" <> show (what, r)) $
        Leaf a
fill v what (Node qu)
  = Node $ fill v <$> subdivide what <*> qu


getLocation :: V2 Int -> Quadrant (Region, a) -> Maybe a
getLocation p (Leaf (r, a))
  | trace (show (r, p)) $ containsPoint r p = Just a
  | otherwise = Nothing
getLocation p (Node qu) = asum $ getLocation p <$> (toList qu)


regionify :: QuadTree a -> Quadrant (Region, a)
regionify (Wrapper q r) = apRegion r q


apRegion :: Region -> Quadrant a -> Quadrant (Region, a)
apRegion r (Leaf a) = Leaf (r, a)
apRegion r (Node qu)
  = Node $ apRegion <$> subdivide r <*> qu


showTree :: (a -> Char) -> QuadTree a -> String
showTree f q@(Wrapper _ (Region x y w h)) =
  unlines $ do
    let q' = regionify q
    yp <- [y .. y + h - 1]
    pure $ do
      xp <- [x .. x + w - 1]
      let p = V2 xp yp
      case getLocation p q' of
        Nothing -> error $ "indexed a bad spot! " <> show p
        Just a -> pure $ f a


regionSize :: Region -> Int
regionSize (Quad _ _ w h) = w * h


asWeighted :: Show a => QuadTree a -> [a]
asWeighted =  join . fmap (uncurry replicate . first regionSize) . toList . regionify


makeTree :: Region -> a -> QuadTree a
makeTree r a = Wrapper (pure a) r


liftTree :: (Quadrant (Region, a) -> Quadrant a) -> QuadTree a -> QuadTree a
liftTree f w = w { wrappedTree = f $ regionify w }

main :: IO ()
main = props

-- TODO(sandy): there is a bug somewhere :(
test :: IO ()
test
  = putStrLn
  $ showTree (bool '.' 'X')
  $ liftTree (fill True $ Region 0 0 2 2)
  $ makeTree (Region 0 0 3 3) False


-- _tl :: Lens' (Quadrant a) (Quadrant a)
-- _tl f l@(Leaf _) = fmap (\x -> Node x l l l) (f l)
-- _tl f (Node tl _ _ _) = f tl

-- _tr :: Lens' (Quadrant a) (Quadrant a)
-- _tr f l@(Leaf _) = fmap (\x -> Node l x l l) (f l)
-- _tr f (Node _ tr _ _) = f tr

-- _bl :: Lens' (Quadrant a) (Quadrant a)
-- _bl f l@(Leaf _) = fmap (\x -> Node l l x l) (f l)
-- _bl f (Node _ _ bl _) = f bl

-- _br :: Lens' (Quadrant a) (Quadrant a)
-- _br f l@(Leaf _) = fmap (\x -> Node l l l x) (f l)
-- _br f (Node _ _ _ br) = f br

-- _wrappedTree :: Lens' (QuadTree a) (Quadrant a)
-- _wrappedTree f q =  (\x -> q { wrappedTree = x }) <$> f (wrappedTree q)

-- -- getLocation :: V2 Int -> QuadTree a -> a
-- -- getLocation = _

