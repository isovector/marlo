{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

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
import Data.String (IsString)
import Network.HTTP.Client (Manager)
import Hasql.Connection (Connection)


data Env = Env
  { e_uri  :: URI
  , e_mgr  :: Manager
  , e_conn :: Connection
  }

type Ranker = ScraperT Text (ReaderT Env IO)


data Link a = Link
  { l_text :: Text
  , l_uri :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


data Stuff = Stuff
  { s_num_links :: Int
  , s_num_gifs :: Int
  , s_num_images :: Int
  , s_num_scripts :: Int
  , s_author :: Maybe Text
  , s_ratio :: (Int, Int)
  , s_sticky :: Bool
  , s_numTweets :: Int
  , s_bootstrap :: Bool
  , s_googleAnalytics :: Bool
  , s_googleAds :: Bool
  , s_num_forms :: Int
  , s_perc_roman :: Double
  } deriving (Eq, Ord, Show)


data Search a
  = Term a
  | Phrase [a]
  | Negate (Search a)
  | And (Search a) (Search a)
  | Or (Search a) (Search a)
  | SiteLike Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

