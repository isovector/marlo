{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

module Signals.Content where

import           Control.Monad ((<=<), join)
import           DB (PageContent (PageContent), Identity)
import           Data.Foldable (fold)
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Lasercutter.HTML
import           Network.URI (URI, parseURI, parseURIReference)
import           Rel8.StateMask (BitMask, flagIf)
import           Signals.Listicle (isListicle)
import           Signals.Schema
import           Types (DocumentFlag(..))


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


canonical :: HtmlParser (Maybe URI)
canonical
  = fmap (listToMaybe . catMaybes)
  $ target ("link" /\ (== Just "canonical") . getAttr "rel")
  $ fmap (parseURI . T.unpack =<<)
  $ proj (getAttr "href")


title :: HtmlParser Text
title = targetOne "title" text


links :: HtmlParser (Set URI)
links
  = fmap S.fromList
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
    ]
-- /* MonsterInsights Scroll Tracking */ in script
-- also kill any tagmanager or analytics???

isSpiritualPollution :: URI -> HtmlParser (BitMask DocumentFlag)
isSpiritualPollution uri = fmap fold $ sequenceA $
  [ canBeFilteredOutBySchemaType uri
  , flagIf HasAds      <$> hasGoogleAds
  , flagIf IsPaywalled <$> hasPaywall
  , flagIf IsListicle  <$> isListicle
  ]

-- there are also FEATURES
-- `span.MathJax` for has math
-- `code` for has code

