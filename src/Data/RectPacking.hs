{-# LANGUAGE RecordWildCards #-}

module Data.RectPacking
  ( Rect (..)
  , V2 (..)
  , place
  , measureText
  , sequenceTile
  , QuadTree
  , makeTree
  , tile
  , tmap
  , Tile
  , Region
  ) where

import Data.QuadTree
import Data.Bifunctor (second)
import Linear.V2
import Control.Lens ((^.))
import Linear hiding (trace)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)


data Rect a = Rect
  { r_pos    :: !(V2 Float)
  , r_size   :: !(V2 Int)
  , r_data   :: !a
  }
  deriving (Eq, Ord, Show, Functor)


rectCenterLoc :: Rect a -> Location
rectCenterLoc = unpackV2 . fmap round . rectCenter

rectCenter :: Rect a -> V2 Float
rectCenter Rect{..} = r_pos + fmap fromIntegral r_size / 2

------------------------------------------------------------------------------
-- | Take a size and a centerpoint, get the top left corner
uncenter :: V2 Int -> V2 Float -> V2 Float
uncenter sz center = center - fmap fromIntegral sz / 2

unpackV2 :: V2 a -> (a, a)
unpackV2 (V2 x y) = (x, y)

rectToSegs :: Rect a -> [(Location, Rect a)]
rectToSegs r@Rect{..} = do
  dx <- [0 .. r_size ^. _x]
  dy <- [0 .. r_size ^. _y]
  pure (unpackV2 $ fmap round $ r_pos + fmap fromIntegral (V2 dx dy), r)


place :: Eq a => Rect a -> QuadTree (Maybe (Rect a)) -> QuadTree (Maybe (Rect a))
place r qt = case safeGetLocation (rectCenterLoc r) qt of
  Nothing -> foldr (uncurry safeSetLocation) qt $ fmap (second Just) $ rectToSegs r
  Just re -> place (offsetBy r re) qt

safeGetLocation :: Eq a => Location -> QuadTree (Maybe a) -> Maybe a
safeGetLocation l q = case outOfBounds l q of
  False -> getLocation l q
  True -> trace ("uh oh: " <> show l) Nothing

safeSetLocation :: Eq a => Location -> a -> QuadTree a -> QuadTree a
safeSetLocation l a q = case outOfBounds l q of
  False -> setLocation l a q
  True -> trace ("uh oh: " <> show l) q

------------------------------------------------------------------------------
-- | Get the length of a vector from the center point  to the end of the size
-- envelope
extent :: V2 Float -> V2 Int -> Float
extent dir (fmap fromIntegral -> V2 w h) =
  norm $ V2 (dot dir $ V2 (w / 2) 0) (dot dir $ V2 0 (h / 2))


offsetBy :: Rect a -> Rect a -> Rect a
offsetBy want collide =
  let dir = normalize $ rectCenter want - rectCenter collide
      get_ext = extent dir . r_size
      new_center = rectCenter want + dir ^* (get_ext want + get_ext collide)
   in want { r_pos = uncenter (r_size want) new_center }

sequenceTile :: Applicative f => Tile (f a) -> f (Tile a)
sequenceTile (fa, b) = (,) <$> fa <*> pure b


measureText :: Text -> V2 Int
measureText s = V2 (T.length s) 1

