{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

module Signals.Content where

import           Control.Arrow ((***))
import           Control.Monad ((<=<), join)
import           DB (PageContent (PageContent), Identity)
import           Data.Foldable (fold)
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Lasercutter.HTML
import           Network.URI (URI, parseURI, parseURIReference, relativeTo, escapeURIString, isUnescapedInURI)
import           Rel8.StateMask (BitMask, flagIf)
import           Signals.AcceptableURI (isAcceptableLink)
import           Signals.Listicle (isListicle)
import           Signals.Schema
import           Types


mainContent :: HtmlParser Text
mainContent
  = asum
  $ fmap (\sel -> targetOne sel $ failIfEmpty contentText)
    [
      "div" /\ ".entry-content"
    , "div" /\ ".content"
    , "div" /\ ".pjgm-postcontent"
    , "div" /\ ".PostsPage-postContent"
    , "div" /\ ".ArticleBody-articleBody"
    , "div" /\ "#mw-content-text"
    , "article"
    , "main"
    , "div" /\ #content
    , "td" /\ ".mainsection"
    , "body"
    ]


headingsContent :: HtmlParser Text
headingsContent
  = fmap (flatten . join)
  $ sequenceA
    [ match "h1" text
    , match "h2" text
    ]


commentsContent :: HtmlParser Text
commentsContent
  = fmap (fromMaybe "")
  $ try
  $ asum
  $ fmap (\sel -> targetOne sel $ failIfEmpty contentText)
    [ "div" /\ ".comments-area"
    , "div" /\ ".comments-page"
    , "div" /\ ".comments"
    , "div" /\ ".PostsPage-commentsSection"
    , "div" /\ "#comments"
    ]


rankContent :: HtmlParser (PageContent Identity)
rankContent = PageContent
  <$> headingsContent
  <*> mainContent
  <*> commentsContent


canonical :: HtmlParser URI
canonical
  = expect
  $ fmap (listToMaybe . catMaybes)
  $ target ("link" /\ (== Just "canonical") . getAttr "rel")
  $ fmap (parseURI . T.unpack =<<)
  $ proj (getAttr "href")


title :: HtmlParser Text
title = targetOne "title" text


links :: HtmlParser (Set URI)
links
  = fmap (S.filter isAcceptableLink)
  $ fmap S.fromList
  $ fmap catMaybes
  $ target "a"
  $ proj
  $ parseURIReference . T.unpack <=< getAttr "href"


hasGoogleAds :: HtmlParser Bool
hasGoogleAds = fmap or $ match "script" $ do
  src <- proj $ getAttr "src"
  txt <- text
  pure $ or
    [ T.isInfixOf "adsbygoogle" $ fromMaybe "" src
    , T.isInfixOf "adsbygoogle" txt
    , T.isInfixOf "pubads()" txt
    , T.isInfixOf "GTM-" txt
    , T.isInfixOf "googletag" txt
      -- NOTE(sandy): not strictly a "google" ad
    , T.isInfixOf "fbq(" txt
    , T.isInfixOf "moatvideo" txt
    , T.isInfixOf "MonsterInsights" txt
    ]

isSpiritualPollution :: URI -> HtmlParser (BitMask DocumentFlag)
isSpiritualPollution uri = fmap fold $ sequenceA $
  [ canBeFilteredOutBySchemaType uri
  , flagIf HasAds      <$> hasGoogleAds
  , flagIf IsPaywalled <$> hasPaywall
  , flagIf IsListicle  <$> isListicle
  ]


features :: HtmlParser (BitMask DocumentFeature)
features = fmap fold $ sequenceA
  [ flagIf HasCode <$> containsAny "code"
  , flagIf HasMath <$> containsAny ".MathJax"
  , flagIf IsForum <$> isForum
  ]


containsAny :: (Html -> Bool) -> HtmlParser Bool
containsAny s =  fmap or . target s $ pure True


isForum :: HtmlParser Bool
isForum = fmap or $ sequenceA
  [ containsAny $ "meta" /\ maybe False (T.isPrefixOf "vBulletin") . getAttr "generator"
  , containsAny $ "meta" /\ maybe False (T.isPrefixOf "Discourse") . getAttr "generator"
  , containsAny $ "body" /\ #phpbb
  ]


scriptAssets :: URI -> HtmlParser ([URI], Int64)
scriptAssets uri
  = fmap ((catMaybes *** sum) . unzip)
  $ target "script"
  $ liftA2 (,)
      (proj $ normalizeAsset uri <=< getAttr "src")
      (fmap (fromIntegral . T.length) text)


styleAssets :: URI -> HtmlParser ([URI], Int64)
styleAssets uri
  = fmap (catMaybes *** sum)
  $ liftA2 (,)
      (target ("link" /\ (== Just "stylesheet") . getAttr "rel")
        $ proj
        $ normalizeAsset uri <=< getAttr "href")
      (target "style"
        $ fmap (fromIntegral . T.length) text)


normalizeAsset :: URI -> Text -> Maybe URI
normalizeAsset here
  = fmap (flip relativeTo here)
  . parseURIReference
  . escapeURIString isUnescapedInURI
  . T.unpack


tweets :: HtmlParser Int
tweets
  = fmap length
  $ target ("blockquote" /\ ".twitter-tweet")
  $ pure ()


gifs :: HtmlParser Int
gifs
  = fmap length
  $ target ("img" /\ maybe False  (T.isSuffixOf ".gif") . getAttr "src")
  $ pure ()

