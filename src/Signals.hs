{-# LANGUAGE OverloadedStrings #-}

module Signals where

import           Control.Applicative (optional, empty)
import           Control.Monad.Reader
import           Data.List (isSuffixOf)
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Keywords
import           Network.URI
import           Text.HTML.Scalpel
import           Types
import           Utils
import Data.Char (toLower)


gif :: Ranker Text
gif = do
  a <- attr "src" "img"
  case T.isSuffixOf ".gif" a of
    True -> pure a
    False -> empty


uniqueImgs :: Ranker (Set URI)
uniqueImgs
  = fmap ( S.fromList
         . filter (not . freeImage)
         . mapMaybe parseURI
         . fmap T.unpack
         )
  . chroots "img"
  $ attr "src" "img"


freeImage :: URI -> Bool
freeImage uri = fromMaybe False $ do
  auth <- uriAuthority uri
  pure $ isSuffixOf "gravatar.com" $ uriRegName auth


numScripts :: Ranker Int
numScripts = countOf "script"


numForms :: Ranker Int
numForms = countOf "form"


author :: Ranker (Maybe Text)
author = optional $ text $ tagClass "span" "author"


title :: Ranker Text
title = text "title"


titleKeywords :: Ranker [Keyword]
titleKeywords = fmap (mapMaybe keywordify . T.words) title


link :: Ranker (Link URI)
link = do
  here <- asks e_uri
  t    <- T.strip <$> text "a"
  guard $ not $ T.null t
  href <- attr "href" "a"
  case parseURIReference (T.unpack href) of
    Just uri | isAcceptableLink uri ->
      pure $ Link t $ normalizeURI $ relativeTo uri here
    _ -> empty


normalizeURI :: URI -> URI
normalizeURI uri = uri { uriFragment = "" }


isAcceptableLink :: URI -> Bool
isAcceptableLink uri
  | Just auth <- uriAuthority uri = and
      [ any (== uriScheme uri) ["http:", "https:"]
      , not $ any (`isSuffixOf` path)
          [ ".pdf"
          , ".png"
          , ".gif"
          , ".jpg"
          , ".jpeg"
          , ".tif"
          , ".tiff"
          , ".epub"
          , ".zip"
          , ".7g"
          , ".tar"
          , ".doc"
          , ".docx"
          , ".rtf"
          ]
      , not $ any (isOnDomain $ uriRegName auth)
          [ "twitter.com"
          , "facebook.com"
          , "youtube.com"
          , "youtu.be"
          , "pintrest.com"
          , "imgur.com"
          , "instagram.com"
          , "google.com"
          , "amazon.com"
          , "tiktok.com"
          , "snapchat.com"
          , "linkedin.com"
          , "tumblr.com"
          , "archive.org"
          , "vimeo.org"
          ]
      ]
    | otherwise = False
  where
    path = fmap toLower $ uriPath uri


isOnDomain :: String -> String -> Bool
isOnDomain x dom = dom == x || isSuffixOf ('.' : dom) x


links :: Ranker [Link URI]
links = chroots "a" link


headingParaRatio :: Ranker (Int, Int)
headingParaRatio = do
  headings <- fmap join $ for [1..5] $ \i -> chroots (tagSelector $ "h" <> show i) $ pure ()
  paras <- chroots "p" $ pure ()
  pure $ (length paras , length headings)


rankStuff :: Ranker Stuff
rankStuff =
  Stuff
    <$> fmap length links
    <*> fmap length (chroots "img" gif)
    <*> numScripts
    <*> author
    <*> headingParaRatio
    <*> hasSticky


hasSticky :: Ranker Bool
hasSticky = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "sticky"

