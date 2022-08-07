{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Data.RectPacking
  ( Rect (..)
  , V2 (..)
  , place
  , forcePlace
  , measureText
  , rectToRegion
  , QuadTree
  , makeTree
  , tile
  , type Region
  , pattern Region
  ) where

import           Data.Monoid
import           Data.QuadAreaTree
import           Data.Text (Text)
import qualified Data.Text as T
import           Linear hiding (trace)


data Rect a = Rect
  { r_pos    :: !(V2 Float)
  , r_size   :: !(V2 Int)
  , r_data   :: !a
  }
  deriving (Eq, Ord, Show, Functor)


rectCenter :: Rect a -> V2 Float
rectCenter Rect{..} = r_pos + fmap fromIntegral r_size / 2


------------------------------------------------------------------------------
-- | Take a size and a centerpoint, get the top left corner
uncenter :: V2 Int -> V2 Float -> V2 Float
uncenter sz center = center - fmap fromIntegral sz / 2


place :: Eq a => Rect a -> QuadTree (Maybe (Rect a)) -> QuadTree (Maybe (Rect a))
place = go (0 :: Int)
  where
    go 4 _ qt = qt
    go n r qt =
      case getFirst $ hitTest First (rectToRegion r) qt of
        Nothing -> fillRect r qt
        Just re ->
          go (n + 1) (offsetBy r re) qt



forcePlace :: Eq a => Rect a -> QuadTree (Maybe (Rect a)) -> QuadTree (Maybe (Rect a))
forcePlace r qt = fillRect r qt

rectToRegion :: Rect a -> Region
rectToRegion Rect{r_pos = fmap round -> V2 x y, r_size = V2 w h } = Region x y w h


fillRect :: Rect a -> QuadTree (Maybe (Rect a)) -> QuadTree (Maybe (Rect a))
fillRect r = fill (Just r) $ rectToRegion r


------------------------------------------------------------------------------
-- | Get the length of a vector from the center point  to the end of the size
-- envelope
extent :: V2 Float -> V2 Int -> Float
extent dir (fmap fromIntegral -> V2 w h) =
  norm $ V2 (dot dir $ V2 (w / 2) 0) (dot dir $ V2 0 (h / 2))


offsetBy :: Rect a -> Rect a -> Rect a
offsetBy want collide =
  let dir = unzero $ normalize $ rectCenter want - rectCenter collide
      get_ext = extent dir . r_size
      new_center = rectCenter want + dir ^* ((get_ext want + get_ext collide) * 1.5)
      res = want { r_pos = uncenter (r_size want) new_center }
   in -- trace (show ("offsetting ", r_pos want, " to ", r_pos res, dir ))
      res

unzero :: V2 Float -> V2 Float
unzero v | quadrance v < 0.1  = V2 0 1
unzero v = v


measureText :: Text -> V2 Int
measureText s = V2 (T.length s + 1) 1
-- plus one for the icon

