{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Data.RectPacking
  ( Rect (..)
  , V2 (..)
  , place
  , measureText
  -- , sequenceTile
  , QuadTree
  , makeTree
  , tile
  , type Region
  , pattern Region
  ) where

import           Control.Monad (join)
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


rectCenterLoc :: Rect a -> V2 Int
rectCenterLoc = fmap round . rectCenter

rectCenter :: Rect a -> V2 Float
rectCenter Rect{..} = r_pos + fmap fromIntegral r_size / 2

------------------------------------------------------------------------------
-- | Take a size and a centerpoint, get the top left corner
uncenter :: V2 Int -> V2 Float -> V2 Float
uncenter sz center = center - fmap fromIntegral sz / 2


place :: Eq a => Rect a -> QuadTree (Maybe (Rect a)) -> QuadTree (Maybe (Rect a))
place r qt = case join $ getLocation qt $ rectCenterLoc r of
  Nothing -> fillRect r qt
  Just re -> place (offsetBy r re) qt


fillRect :: Rect a -> QuadTree (Maybe (Rect a)) -> QuadTree (Maybe (Rect a))
fillRect r@Rect{r_pos = fmap round -> V2 x y, r_size = V2 w h }
  = fill (Just r) $ Region x y w h


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

-- sequenceTile :: Applicative f => (f a) -> f (Tile a)
-- sequenceTile (fa, b) = (,) <$> fa <*> pure b


measureText :: Text -> V2 Int
measureText s = V2 (T.length s) 1

