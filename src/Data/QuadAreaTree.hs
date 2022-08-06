{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
module Data.QuadAreaTree where

import Linear.V2
import Control.Applicative
import Data.Foldable (asum, toList, traverse_)
-- import Debug.Trace (trace)
import Debug.Trace (trace)
import Text.Show.Pretty (pPrint)
import Test.QuickCheck
import Data.Bool (bool)

-- trace = flip const

data QuadTree a = Wrapper
  { wrappedTree :: Quadrant a
    -- | Must be a power of two!!
  , quadSize :: Region
  }
  deriving (Show, Read, Eq, Functor)


type Region = Quad Int

pattern Region :: Int -> Int -> Int -> Int -> Region
pattern Region { r_x, r_y, r_w, r_h } = Quad r_x r_y r_w r_h
{-# COMPLETE Region #-}

data Quad a
  = Quad !a !a !a !a
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance Applicative Quad where
  pure a = Quad a a a a
  Quad ftl ftr fbl fbr <*> Quad atl atr abl abr =
    Quad (ftl atl) (ftr atr) (fbl abl) (fbr abr)


subdivide :: Region -> Maybe (Quad Region)
subdivide (Region _ _ w h)
  | w < 2 || h < 2
  = Nothing
subdivide (Region x y w h) =
  let halfw = div w 2
      halfh = div h 2
   in Just $ Quad
        (Region x y halfw halfh)
        (Region (x + halfw) y (w - halfw) halfh)
        (Region x (y + halfh) halfw (h - halfh))
        (Region (x + halfw) (y + halfh) (w - halfw) (h - halfh))

instance Arbitrary Region where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    Positive w <- fmap (fmap (+ 1)) arbitrary
    Positive h <- fmap (fmap (+ 1)) arbitrary
    pure $ Region x y w h

prop_gen_subdivide :: Testable prop => (Region -> Quad Region -> prop) -> Property
prop_gen_subdivide f = property $ do
  r <- arbitrary
  let Just q = subdivide r
  pure
    $ counterexample (show r)
    $ counterexample (show q)
    $ f r q


props :: IO ()
props = do
  traverse_ quickCheck
    [ prop_gen_subdivide $
        \(Region x y w h) (Quad (Region x' y' _ _) _ _ _) ->
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

    ]


containsRegion :: Region -> Region -> Bool
containsRegion (Region bx by bw bh) (Region sx sy sw sh) =
  and
    [ bx <= sx
    , by <= sy
    , sx + sw < bx + bw
    , sy + sh < by + bh
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



fill :: a -> Region -> Quadrant (Region, a) -> Quadrant a
fill v what q@(Leaf (r, a)) =
  if
      -- The entire space is covered by what
    | containsRegion what r || what == r -> Leaf v
      -- The space entirely contanis what
    | containsRegion r what -> -- trace ("splits a node: " <> show (what, r)) $
        case subdivide r of
          Nothing -> fmap snd q
          Just subr -> Node $
            fill v
              <$> pure what
              <*> fmap (pure . (, a)) subr
    | intersects what r -> -- trace ("recursing: " <> show (what, r)) $
        case (,) <$> subdivide what <*> subdivide r of
          Nothing -> fmap snd q
          Just (subwhat, subr) -> Node $
            fill v
              <$> subwhat
              <*> fmap (pure . (, a)) subr
    | otherwise -> Leaf a
fill v what q@(Node qu)
  | Just sub <- subdivide what
  = Node $ fill v <$> sub <*> qu
  | otherwise = fmap snd q


getLocation :: V2 Int -> Quadrant (Region, a) -> Maybe a
getLocation p (Leaf (r, a))
  | containsPoint r p = Just a
  | otherwise = Nothing
getLocation p (Node qu) = asum $ getLocation p <$> (toList qu)

regionify :: QuadTree a -> Quadrant (Region, a)
regionify (Wrapper q r) = apRegion r q

apRegion :: Region -> Quadrant a -> Quadrant (Region, a)
apRegion r (Leaf a) = Leaf (r, a)
apRegion r (Node qu)
  | Just sub <- subdivide r
  = Node $ apRegion <$> sub <*> qu
  | otherwise
  = error "ran out of subdivide"

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

makeTree :: Region -> a -> QuadTree a
makeTree r a = Wrapper (pure a) r

liftTree :: (Quadrant (Region, a) -> Quadrant a) -> QuadTree a -> QuadTree a
liftTree f w = w { wrappedTree = f $ regionify w }

-- TODO(sandy): there is a bug somewhere :(
main :: IO ()
main
  = putStrLn
  $ showTree (bool '.' 'X')
  $ liftTree (fill True $ Region 2 2 6 6)
  $ makeTree (Region 0 0 8 8) False


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

