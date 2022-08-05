{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Functor.Identity
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString)
import           Data.Text (Text)
import           Hasql.Connection (Connection)
import           Network.HTTP.Client (Manager)
import           Network.HTTP.Types.Header (ResponseHeaders)
import           Network.URI
import           Text.HTML.Scalpel


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

data Download f a = Download
  { d_mime :: f ByteString
  , d_headers :: ResponseHeaders
  , d_body :: a
  }
  deriving (Functor)

sequenceDownload :: Applicative f => Download f a -> f (Download Identity a)
sequenceDownload (Download m h a) =
  Download
    <$> fmap Identity m
    <*> pure h
    <*> pure a

data Search a
  = Term a
  | Phrase [a]
  | Negate (Search a)
  | And (Search a) (Search a)
  | Or (Search a) (Search a)
  | SiteLike Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

