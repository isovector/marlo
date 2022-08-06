{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE PatternSynonyms     #-}

module Data.QuadAreaTree.Tests where

import Control.Monad (guard)
import Data.Foldable (traverse_)
import Data.Proxy
import Data.QuadAreaTree
import GHC.Generics (Generic)
import GHC.TypeLits (Nat, KnownNat, natVal)
import Linear.V2
import Test.QuickCheck


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

    , property $ \r -> getIntersection r r == Just r

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
        x' <- elements [0 .. w - 1]
        y' <- elements [0 .. h - 1]
        let tree = makeTree r False
        pure $
          length (filter id $ asWeighted $ liftTree (insert True $ V2 (x + x') (y + y')) tree) == 1

    , property $ \(RegionAtLeast (Region x y ((+ 1) -> w) ((+ 1) -> h)) :: RegionAtLeast 2 2) -> do
        x' <- elements [0 .. w - 2]
        y' <- elements [0 .. h - 2]
        let tree = makeTree (Region x y w h) False
            res = liftTree (fill True $ Region (x + x') (y + y') 2 2) tree

        pure $
          counterexample (show x') $
          counterexample (show y') $
          counterexample (show tree) $
          counterexample (show $ length (filter id $ asWeighted res)) $
          length (filter id $ asWeighted res) == 4


    ]

