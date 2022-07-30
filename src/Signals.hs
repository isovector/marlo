{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Signals where

import           Control.Applicative (optional, empty, liftA2, many, (<|>))
import           Control.Monad.Reader
import           Data.List (isSuffixOf, partition, dropWhileEnd, isPrefixOf, isInfixOf)
import           Data.Maybe (mapMaybe, fromMaybe, catMaybes)
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
import Data.Char (toLower, isAlpha, isDigit)
import Debug.Trace (traceM)
import Data.Int (Int64)
import Assets (getAssetSizes)
import Data.Foldable (asum)


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


-- titleKeywords :: Ranker [Keyword]
-- titleKeywords = fmap (mapMaybe keywordify . T.words) title


link :: Ranker (Link URI)
link = do
  here <- asks e_uri
  t    <- T.strip <$> text "a"
  guard $ not $ T.null t
  href <- attr "href" "a"
  fmap (Link t) $ normalizeLink href

normalizeLink :: Text -> Ranker URI
normalizeLink href = do
  here <- asks e_uri
  case parseURIReference (T.unpack href) of
    Just (flip relativeTo here -> uri) | isAcceptableLink uri ->
      pure $ normalizeURI uri
    x -> empty

normalizeAsset :: Text -> Ranker URI
normalizeAsset href = do
  here <- asks e_uri
  case parseURIReference (escapeURIString isUnescapedInURI $ T.unpack href) of
    Just (flip relativeTo here -> uri) -> pure uri
    x -> empty


normalizeURI :: URI -> URI
normalizeURI uri = uri
  { uriAuthority =
      fmap (\x -> x { uriRegName = canonicalDomains $ uriRegName x } ) $ uriAuthority uri
  , uriPath = dropWhileEnd (== '/') $ uriPath uri
  , uriFragment = ""
  , uriQuery = ""
  }

canonicalDomains :: String -> String
canonicalDomains
  = T.unpack
  . T.replace "m.wikipedia.org" "wikipedia.org"
  . T.replace "new.reddit.com" "old.reddit.com"
  . T.pack

hasBootstrap :: Ranker Bool
hasBootstrap = fmap or $ chroots "link" $ do
  href <- attr "href" "link"
  pure $ T.isInfixOf "bootstrap.min" href

hasGoogleAnalytics :: Ranker Bool
hasGoogleAnalytics = fmap or $ chroots "script" $ do
  t <- text "script"
  pure $ any (flip T.isInfixOf t)
    [ "GoogleAnalyticsObject"
    , "google-analytics"
    , "googleAnalyticsCode"
    , "UA-"
    ]

tweet :: Ranker Bool
tweet = do
  c <- attr "class" "blockquote"
  pure $ c == "twitter-tweet"

tweets :: Ranker Int
tweets = fmap length $ chroots "blockquote" tweet

-- iframe to youtube

isAcceptableLink :: URI -> Bool
isAcceptableLink uri
  | Just auth <- uriAuthority uri = and
      [ any (== uriScheme uri) ["http:", "https:"]
      , not $ any (`isSuffixOf` path)
          [ ".pdf"
          , ".png"
          , ".gif"
          , ".js"
          , ".css"
          , ".jpg"
          , ".jpeg"
          , ".tif"
          , ".tiff"
          , ".epub"
          , ".zip"
          , ".tgz"
          , ".7g"
          , ".tar"
          , ".doc"
          , ".docx"
          , ".rtf"
          , ".mp4"
          , ".mp3"
          , ".mpv"
          , ".avi"
          , ".ogg"
          , ".mkv"
          , ".bmp"
          , ".gz"
          , ".py"
          , ".xls"
          , ".xlsl"
          , ".ppt"
          , ".ipyn"
          , ".ipynb"
          ]
      , not $ any (isOnDomain $ uriRegName auth) forbidSites
      , not (isOnDomain (uriRegName auth) "wikipedia.org")
          || (isOnDomain (uriRegName auth) "wikipedia.org"
              && isOnDomain (uriRegName auth) "en.wikipedia.org")
      , specificAllowRules uri
      ]
    | otherwise = False
  where
    path = fmap toLower $ uriPath uri


(==>) :: Bool -> Bool -> Bool
(==>) x y = not x || y

forbidSites :: [String]
forbidSites =
  [ "twitter.com"
  , "facebook.com"
  , "youtube.com"
  , "youtu.be"
  , "pintrest.com"
  , "imgur.com"
  , "instagram.com"
  , "google.com"
  , "archive.today"
  , "amazon.com"
  , "flickr.com"
  , "spotify.com"
  , "last.fm"
  , "goo.gl"
  , "goodreads.com"
  , "ghostarchive.org"
  , "wp.me"
  , "reddit.com"
  , "tiktok.com"
  , "snapchat.com"
  , "spoilertv.com"
  , "bloomberg.com"
  , "linkedin.com"
  , "tumblr.com"
  , "scribd.com"
  , "archive.org"
  , "vimeo.com"
  , "tinyurl.com"
  , "mega.nz"
  , "anonfiles.com"
  , "mediafire.com"
  , "gofile.io"
  , "archive.today"
  , "forbes.com"
  , "gist.github.com"
  ]


forbidPaths :: [String]
forbidPaths =
  [ "/tag/"
  , "/tags/"
  , "/tagged/"
  , "/search/"
  , "/category/"
  , "/categories/"
  , "/collections/"
  , "/topic/"
  , "/product/"
  , "/products/"
  , "/feed/"
  , "/feeds/"
  , "/author/"
  , "/authors/"
  , "/news/"
  , "/politics/20"
  ]


specificAllowRules :: URI -> Bool
specificAllowRules uri
  | Just auth <- uriAuthority uri =
    let on_domain = isOnDomain (uriRegName auth)
     in not $ or $
  -- Succeeds if none of the following are true
  fmap (flip isInfixOf path) forbidPaths <>
  [ on_domain "github.com" && isInfixOf "/commit/" path
  , on_domain "github.com" && isInfixOf "/commits/" path
  , on_domain "github.com" && isInfixOf "/blob/" path
  , on_domain "github.com" && isInfixOf "/edit/" path
  , isYearMonthPage path
  ]
  | otherwise = error "yo"
  where
    path = fmap toLower $ uriPath uri

isYearMonthPage :: String -> Bool
isYearMonthPage p =
  case fmap T.unpack $ T.split (== '/') $ T.pack p of
    ["", year@(y:_:_:_:[]), month@(_:_:[]), ""]
      | all isDigit year
      , all isDigit month
      , elem y ['1', '2']
      -> True
    ["", year@(y:_:_:_:[]), month@(_:_:[])]
      | all isDigit year
      , all isDigit month
      , elem y ['1', '2']
      -> True
    ["", year@(y:_:_:_:[]), ""]
      | all isDigit year
      , elem y ['1', '2']
      -> True
    ["", year@(y:_:_:_:[])]
      | all isDigit year
      , elem y ['1', '2']
      -> True
    _ -> False



jsBundleSize :: Ranker Int64
jsBundleSize = do
  s <- chroots "script" $ attr "src" "script"
  inline <- texts "script"
  ls <- traverse normalizeAsset s
  (mgr, conn) <- asks $ (,) <$> e_mgr <*> e_conn
  szs <- liftIO $ getAssetSizes mgr conn $ fmap (T.pack . show) ls
  pure $ sum szs + sum (fmap (fromIntegral . T.length) inline)

cssBundleSize :: Ranker Int64
cssBundleSize = do
  s <-
    chroots "link" $ do
      rel <- attr "rel" "link"
      guard $ rel == "stylesheet"
      attr "href" "link"
  inline <- texts "style"
  ls <- traverse normalizeAsset s
  (mgr, conn) <- asks $ (,) <$> e_mgr <*> e_conn
  szs <- liftIO $ getAssetSizes mgr conn $ fmap (T.pack . show) ls
  pure $ sum szs + sum (fmap (fromIntegral . T.length) inline)


isOnDomain :: String -> String -> Bool
isOnDomain x dom = dom == x || isSuffixOf ('.' : dom) x


links :: Ranker [Link URI]
links = chroots "a" link


paraHeadingRatio :: Ranker (Int, Int)
paraHeadingRatio = do
  headings <- fmap join $ for [1..5] $ \i -> chroots (tagSelector $ "h" <> show i) $ pure ()
  paras <- chroots "p" $ pure ()
  pure $ (length paras, length headings)


romanPage :: Ranker Double
romanPage = do
  ts <- fmap (filter isAlpha . T.unpack . T.concat) $ texts "p"
  let (non, rs) = partition (not . isKeywordLetter) ts
      lnon = fromIntegral $ length non
      lrs = fromIntegral $ length rs
  pure $ lrs / (lnon + lrs)


hasGoogleAds :: Ranker Bool
hasGoogleAds = fmap or $ chroots "script" $ do
  src <- attr "src" "script"
  pure $ T.isInfixOf "adsbygoogle" src

rankStuff :: Ranker Stuff
rankStuff =
  Stuff
    <$> fmap length links
    <*> fmap length (chroots "img" gif)
    <*> fmap length uniqueImgs
    <*> numScripts
    <*> author
    <*> paraHeadingRatio
    <*> hasSticky
    <*> tweets
    <*> hasBootstrap
    <*> hasGoogleAnalytics
    <*> hasGoogleAds
    <*> numForms
    <*> romanPage


hasSticky :: Ranker Bool
hasSticky = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "sticky"

hasModal :: Ranker Bool
hasModal = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "modal"


textsWithoutScripts :: Selector -> Ranker [Text]
textsWithoutScripts sel = chroot sel $ textWithoutScripts

textWithoutScripts :: Ranker [Text]
textWithoutScripts = fmap (fmap T.strip) $ inSerial $ many $ stepNext innerScraper
  where
    innerScraper :: Ranker Text
    innerScraper = plainText
               <|> skip
               <|> fmap (T.intercalate " ") unknown
               <|> pure "YO"

    plainText :: Ranker Text
    plainText  = text (textSelector `atDepth` 0)

    skipMe :: Selector -> Ranker Text
    skipMe what = "" <$ recurseOn what

    skip :: Ranker Text
    skip     = asum
      [ skipMe "style"
      , skipMe "script"
      , skipMe "noscript"
      , skipMe "nav"
      , skipMe "h1"
      , skipMe "h2"
      , skipMe "h3"
      , skipMe "h4"
      , skipMe "h5"
      , skipMe "h6"
      , skipMe "sup"
      , skipMe "sub"
      ]

    unknown   = recurseOn anySelector

    recurseOn tag = do
      traceM "recursing"
      chroot (tag `atDepth` 0) $ textWithoutScripts


mainContent :: Ranker Text
mainContent = fmap (T.intercalate " ") $
  asum
  [ failIfEmpty $ textsWithoutScripts $ tagClass "div" "entry-content"
  , failIfEmpty $ textsWithoutScripts $ tagClass "div" "content"
  , failIfEmpty $ textsWithoutScripts $ tagClass "div" "pjgm-postcontent"
  , failIfEmpty $ textsWithoutScripts $ tagClass "div" "PostsPage-postContent"
  , failIfEmpty $ textsWithoutScripts $ tagClass "div" "ArticleBody-articleBody"
  , failIfEmpty $ textsWithoutScripts $ tagId "div" "mw-content-text"
  , failIfEmpty $ textsWithoutScripts "article"
  , failIfEmpty $ textsWithoutScripts "main"
  , failIfEmpty $ textsWithoutScripts $ tagId "div" "content"
  , failIfEmpty $ textsWithoutScripts $ tagClass "td" "mainsection"
  , failIfEmpty $ textsWithoutScripts "body"
  ]

failIfEmpty :: Monad m => ScraperT z m [a] -> ScraperT z m [a]
failIfEmpty m = do
  m >>= \case
    [] -> empty
    x -> pure x


headingsContent :: Ranker Text
headingsContent = fmap (T.intercalate " ") $
  liftA2 (<>) <$> texts "h1" <*> texts "h2"


commentsContent :: Ranker Text
commentsContent = fmap (T.intercalate " ") $ asum
  [ failIfEmpty $ textsWithoutScripts $ tagClass "div" "comments-area"
  , failIfEmpty $ textsWithoutScripts $ tagClass "div" "comments-page"
  , failIfEmpty $ textsWithoutScripts $ tagClass "div" "comments"
  , failIfEmpty $ textsWithoutScripts $ tagClass "div" "PostsPage-commentsSection"
  , failIfEmpty $ textsWithoutScripts $ tagId "div" "comments"
  , pure []
  ]

