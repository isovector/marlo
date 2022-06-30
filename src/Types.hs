{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Types where

import           Control.Monad.Reader
import           Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as M
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import           Network.URI
import           Text.HTML.Scalpel


data Env = Env
  { e_uri :: URI
  }

type Ranker = ScraperT Text (Reader Env)


data Link = Link
  { l_text :: Text
  , l_uri :: URI
  }
  deriving (Eq, Ord, Show)


data Stuff = Stuff
  { s_num_links :: Int
  , s_num_gifs :: Int
  , s_num_scripts :: Int
  , s_author :: Maybe Text
  , s_ratio :: (Int, Int)
  , s_sticky :: Bool
  } deriving (Eq, Ord, Show)


newtype InverseIndex = InverseIndex
  { getInverseIndex :: MonoidalMap Text [URI]
  } deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

