{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module QuadAreaTreeSpec where

import Control.Monad (guard)
import Data.Proxy
import Data.QuadAreaTree
import Data.QuadAreaTree.Internal hiding (getLocation, insert, fill)
import Data.QuadAreaTree.Geometry
import GHC.Generics (Generic)
import GHC.TypeLits (Nat, KnownNat, natVal)
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Classes (foldable, functor, applicative, semigroup, monoid)
import Test.QuickCheck.Checkers (TestBatch, EqProp, (=-=))
import Data.Foldable (for_)
import Data.Monoid (Sum)

getNatVal :: forall a. KnownNat a => Int
getNatVal = fromInteger $ natVal $ Proxy @a


validRegion :: forall w h. (KnownNat w, KnownNat h) => RegionAtLeast w h -> Bool
validRegion (RegionAtLeast (Region _ _ w h)) =
  w >= getNatVal @w && h >= getNatVal @h


newtype RegionAtLeast (w :: Nat) (h :: Nat) = RegionAtLeast
  { getRegionAtLeast :: Region
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary a => Arbitrary (Quad a) where
  arbitrary = Quad <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

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

instance Arbitrary a => Arbitrary (QuadTree a) where
  arbitrary = do
    r <- arbitrary
    x <- makeTree <$> pure r <*> arbitrary
    inserts <- scale (flip div 4) $ listOf $ (,) <$> arbitrary <*> subRegion r
    pure $ foldr (uncurry fill) x inserts


deriving via (RegionAtLeast 1 1) instance {-# OVERLAPPING #-} (Arbitrary Region)

instance EqProp a => EqProp (Quad a) where

instance EqProp a => EqProp (Quadrant a) where
  Leaf a  =-= Leaf b   =           a =-= b
  Leaf a  =-= Node qu  = splitLeaf a =-= qu
  Node qu =-= Leaf b   = splitLeaf b =-= qu
  Node qu =-= Node qu' =          qu =-= qu'


instance Arbitrary a => Arbitrary (Quadrant a) where
  arbitrary =
    let terminal = [Leaf <$> arbitrary]
     in sized $ \n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              (Node <$> scale (flip div 4) arbitrary) : terminal
  shrink = genericShrink


prop_gen_subdivide :: Testable prop => String -> (Region -> Quad Region -> prop) -> Spec
prop_gen_subdivide name f = prop name $ do
  r <- arbitrary
  let q = subdivide r
  pure
    $ counterexample (show r)
    $ counterexample (show q)
    $ f r q


spec :: Spec
spec = do
  prop_gen_subdivide "subdivide maintains topleft corner" $
    \(Region x y _ _)
     (Quad (Region x' y' _ _) _ _ _) ->
      x == x' && y == y'

  prop_gen_subdivide "subdivide maintains size" $
    \(Region _ _ w h)
     (Quad (Region _ _ w1 h1) _ _ (Region _ _ w2 h2)) ->
      w == w1 + w2 && h == h1 + h2

  prop_gen_subdivide "subdivide quadrants are sane" $
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

  prop "regions contain themselves" $ \r ->
    containsRegion r r

  prop "regions intersect with themselves" $ \r ->
    intersects r r

  prop "a regions intersection with itself is itself" $ \r ->
    getIntersection r r == Just r

  prop "intersection is commutative" $ \r1 r2 ->
    getIntersection r1 r2 == getIntersection r2 r1

  prop "subdivisions do not intersect" $ \r ->
    let Quad tl tr bl br = subdivide r
     in and $ do
          r1 <- [tl, tr, bl, br]
          r2 <- [tl, tr, bl, br]
          guard $ r1 /= r2
          pure $ not $ intersects r1 r2

  prop "insert adds one value to a QAT" $ \r@(Region x y w h) -> do
    x' <- elements [0 .. w - 1]
    y' <- elements [0 .. h - 1]
    let tree = makeTree r False
    pure $
      length (filter id $ asWeighted $ insert True (V2 (x + x') (y + y')) tree) == 1

  prop "fill 2x2 adds four values to a QAT" $
    \(RegionAtLeast (Region x y ((+ 1) -> w) ((+ 1) -> h)) :: RegionAtLeast 2 2) -> do
      x' <- elements [0 .. w - 2]
      y' <- elements [0 .. h - 2]
      let tree = makeTree (Region x y w h) False
          res = fill True (Region (x + x') (y + y') 2 2) tree

      pure $
        counterexample (show x') $
        counterexample (show y') $
        counterexample (show tree) $
        counterexample (show $ length (filter id $ asWeighted res)) $
        length (filter id $ asWeighted res) == 4

  prop "fill gives you back what you put in" $ \r (v0 :: Int) v -> do
    sub <- subRegion r
    p <- pointInRegion sub
    let tree = fill v sub $ makeTree r v0
    pure $ getLocation tree p == Just v

  describe "quads" $ do
    propBatch $ semigroup   $ (undefined :: (Quad (Sum Int), Int))
    propBatch $ monoid      $ (undefined :: Quad (Sum Int))
    propBatch $ functor     $ (undefined :: Quad (Int, Int, Int))
    propBatch $ applicative $ (undefined :: Quad (Int, Int, Int))

  describe "quadrants" $ do
    propBatch $ functor     $ (undefined :: Quadrant (Int, Int, Int))
    propBatch $ applicative $ (undefined :: Quadrant (Int, Int, Int))

  describe "quadtree" $ do
    propBatch $ foldable    $ (undefined :: QuadTree (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))


propBatch :: TestBatch -> Spec
propBatch (batch, ts) =
  describe batch $ do
    for_ ts $ uncurry prop



pointInRegion :: Region -> Gen (V2 Int)
pointInRegion (Region x y w h) = do
  dx <- elements [0 .. w - 1]
  dy <- elements [0 .. h - 1]
  pure $ V2 (x + dx) (y + dy)


subRegion :: Region -> Gen Region
subRegion r@(Region x0 y0 w0 h0) = do
  V2 x y <- pointInRegion r
  let remx = x0 + w0 - x
      remy = y0 + h0 - y
  w <- elements [1 .. remx]
  h <- elements [1 .. remy]
  pure $ Region x y w h

