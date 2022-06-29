{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import           Control.Applicative (optional, empty, (<|>), liftA2)
import           Control.Monad.Reader
import           Data.Char (isAlphaNum, isLetter)
import           Data.Foldable (for_)
import           Data.List (genericLength, isSuffixOf)
import           Data.Maybe (mapMaybe, fromJust, fromMaybe)
import           Data.Ratio ((%))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable (for)
import           Debug.Trace (traceM)
import           Network.URI
import           Text.HTML.Scalpel


data Env = Env
  { e_uri :: URI
  }

type Ranker = ScraperT Text (Reader Env)


runRanker :: URI -> Text -> Ranker a -> Maybe a
runRanker u t = flip runReader (Env u) . scrapeStringLikeT t

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


countOf :: Selector -> Ranker Int
countOf sel = fmap length $ chroots sel $ pure ()

numScripts :: Ranker Int
numScripts = countOf "script"

numForms :: Ranker Int
numForms = countOf "form"

tagClass :: String -> String -> Selector
tagClass a b = TagString a @: [hasClass b]

classOf :: String -> Selector
classOf c = AnyTag @: [hasClass c]

author :: Ranker (Maybe Text)
author = optional $ text $ tagClass "span" "author"

data Link = Link
  { l_text :: Text
  , l_uri :: URI
  }
  deriving (Eq, Ord, Show)


title :: Ranker Text
title = text "title"

keywordify :: Text -> Maybe Text
keywordify = elim . T.strip . T.filter isLetter . T.toLower
  where
    elim "" = Nothing
    elim "the" = Nothing
    elim "of" = Nothing
    elim "on" = Nothing
    elim "where" = Nothing
    elim "that" = Nothing
    elim "which" = Nothing
    elim "and" = Nothing
    elim "in" = Nothing
    elim x = Just x

titleKeywords :: Ranker [Text]
titleKeywords = fmap (mapMaybe keywordify . T.words) title

link :: Ranker Link
link = do
  t    <- T.strip <$> text "a"
  guard $ not $ T.null t
  href <- attr "href" "a"
  case parseURIReference (T.unpack href) of
    Nothing -> empty
    Just uri -> pure $ Link t uri

links :: Ranker [Link]
links = chroots "a" link

unsafeURI :: String -> URI
unsafeURI = fromJust . parseURI

good :: IO [(URI, Text)]
good = do
  a <- T.readFile "data/good/lw.html"
  b <- T.readFile "data/good/mr.html"
  c <- T.readFile "data/good/sandy.html"
  d <- T.readFile "data/good/moloch.html"
  pure [ (unsafeURI "https://google.com", a)
       , (unsafeURI "https://google.com", b)
       , (unsafeURI "https://google.com", c)
       , (unsafeURI "https://google.com", d)
       ]


currentURI :: Ranker URI
currentURI = asks e_uri

uriKeywords :: URI -> [Text]
uriKeywords = mapMaybe keywordify . T.split (\x -> x ==  '/' || x == '-') . T.pack . uriPath

headingParaRatio :: Ranker (Int, Int)
headingParaRatio = do
  headings <- fmap join $ for [1..5] $ \i -> chroots (tagSelector $ "h" <> show i) $ pure ()
  paras <- chroots "p" $ pure ()
  pure $ (length paras , length headings)


data Stuff = Stuff
  { s_num_links :: Int
  , s_num_gifs :: Int
  , s_num_scripts :: Int
  , s_author :: Maybe Text
  , s_ratio :: (Int, Int)
  , s_sticky :: Bool
  } deriving (Eq, Ord, Show)

rankStuff :: Ranker Stuff
rankStuff =
  Stuff
    <$> fmap length links
    <*> fmap length (chroots "img" gif)
    <*> numScripts
    <*> author
    <*> headingParaRatio
    <*> hasSticky

withClass :: Selector -> (Text -> Bool) -> Ranker ()
withClass s f = do
  cls <- T.words <$> attr "class" s
  guard $ any f cls

has :: Ranker a -> Ranker Bool
has r = True <$ r <|> pure False


hasSticky :: Ranker Bool
hasSticky = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "sticky"


bad :: IO [(URI, Text)]
bad = do
  a <- T.readFile "data/bad/travel.html"
  b <- T.readFile "data/bad/recipe.html"
  c <- T.readFile "data/bad/buzzfeed.html"
  pure [ (unsafeURI "https://google.com", a)
       , (unsafeURI "https://www.thechunkychef.com/family-favorite-baked-mac-and-cheese/", b)
       , (unsafeURI "https://google.com", c)
       ]

main :: IO ()
main = do
  let r = fmap S.size uniqueImgs

  putStrLn "GOOD"
  goods <- good
  for_ goods $ \(uri, txt) -> do
    print $ runRanker uri txt r

  putStrLn ""
  putStrLn "BAD"
  bads <- bad
  for_ bads $ \(uri, txt) -> do
    print $ runRanker uri txt r


