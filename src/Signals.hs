{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Signals where

import           Assets (getAssetSizes)
import           Control.Applicative (empty, liftA2, many, (<|>), Alternative)
import           Control.Monad.Reader
import           DB
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (asum, fold)
import           Data.Int (Int64)
import           Data.List (isSuffixOf, genericLength)
import           Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Marlo.URIs (normalizeURI)
import           Network.URI
import           Rel8.StateMask (flagIf, BitMask)
import           Signals.AcceptableURI
import           Signals.Listicle (isListicle)
import           Signals.Schema
import           Text.HTML.Scalpel
import           Types
import           Utils


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


numForms :: Ranker Int
numForms = countOf "form"


title :: Ranker Text
title = text "title"


link :: Ranker (URI)
link = do
  href <- attr "href" "a"
  normalizeLink href


normalizeLink :: Text -> Ranker URI
normalizeLink href = do
  here <- asks e_uri
  case parseURIReference (T.unpack href) of
    Just (flip relativeTo here -> uri) | isAcceptableLink uri ->
      pure $ normalizeURI uri
    _ -> empty


normalizeAsset :: Text -> Ranker URI
normalizeAsset href = do
  here <- asks e_uri
  case parseURIReference (escapeURIString isUnescapedInURI $ T.unpack href) of
    Just (flip relativeTo here -> uri) -> pure uri
    _ -> empty



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


jsBundleSize :: Ranker Int64
jsBundleSize = do
  s <- chroots "script" $ attr "src" "script"
  inline <- texts "script"
  ls <- traverse normalizeAsset s
  (conn) <- asks e_conn
  szs <- liftIO $ getAssetSizes conn $ fmap (T.pack . show) ls
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
  conn <- asks e_conn
  szs <- liftIO $ getAssetSizes conn $ fmap (T.pack . show) ls
  pure $ sum szs + sum (fmap (fromIntegral . T.length) inline)


canonical :: Ranker URI
canonical = do
  lcan <-
    chroots "link" $ do
      rel <- attr "rel" "link"
      guard $ rel == "canonical"
      attr "href" "link"
  maybe empty pure
    $ listToMaybe
    $ mapMaybe (parseURI . T.unpack) lcan


links :: Ranker [URI]
links = fmap nubOrd $ chroots "a" link


hasGoogleAds :: Ranker Bool
hasGoogleAds = fmap or $ chroots "script" $ do
  src <- attr "src" "script"
  pure $ or
    [ T.isInfixOf "adsbygoogle" src
    , T.isInfixOf "pubads()" src
    ]


hasSticky :: Ranker Bool
hasSticky = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "sticky"


hasModal :: Ranker Bool
hasModal = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "modal"
-- #email-popup-main-wrapper
-- https://www.mightynetworks.com/encyclopedia/passion-economy
--
-- div class="wppopups-whole"
-- <div id="newsletter-signup-modal">
-- <div role="dialog" aria-modal="true" id="newsletter-signup-modal-inner" tabindex="0" class="lv-modal newsletter-signup-modal lv-show" instance="popup" style="background-image: url(&quot;https://assetssc.leevalley.com:443/en-ca/-/media/images/01_homepage/feature-content-slider/09-2019/tools_joinery-project_01_fs.jpg?h=1111&amp;la=en-ca&amp;w=2880&amp;revision=6615dd6e-9bdc-4116-9ecc-b2e2dfd506b5&amp;modified=20190515203513&amp;hash=3FEF3022E01D02D167BF02376DEA6F527E5D5B7F&quot;);">


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
      , skipMe "li"
      , skipMe "ul"
      , skipMe "ol"
      , skipMe "iframe"
      , skipMe "nav"
      , skipMe "object"
      , skipMe "source"
      , skipMe "svg"
      , skipMe "template"
      , skipMe "track"
      , skipMe "select"
      , skipMe "option"
      , skipMe "button"
      , skipMe "canvas"
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

    recurseOn tag = chroot (tag `atDepth` 0) $ textWithoutScripts


isSpiritualPollution :: Ranker (BitMask DocumentFlag)
isSpiritualPollution = fmap fold $ sequenceA $
  [ canBeFilteredOutBySchemaType
  , flagIf HasAds      <$> hasGoogleAds
  , flagIf IsPaywalled <$> hasPaywall
  , flagIf IsListicle  <$> isListicle
  ]


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


rankContent :: Ranker (PageContent Identity)
rankContent = PageContent
  <$> headingsContent
  <*> mainContent
  <*> commentsContent


rankStats :: Ranker (PageStats Identity)
rankStats = PageStats
  <$> fmap fromIntegral jsBundleSize
  <*> fmap fromIntegral cssBundleSize
  <*> fmap fromIntegral tweets
  <*> fmap genericLength (chroots "img" gif)
  <*> pure False

