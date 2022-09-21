{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Signals (canonical, rankStats) where

import           Assets (getAssetSizes)
import           Control.Applicative (empty)
import           Control.Monad.Reader
import           DB
import           Data.List (genericLength)
import           Data.Maybe (mapMaybe, listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.URI
import           Text.HTML.Scalpel
import           Types


gif :: Ranker Text
gif = do
  a <- attr "src" "img"
  case T.isSuffixOf ".gif" a of
    True -> pure a
    False -> empty



normalizeAsset :: Text -> Ranker URI
normalizeAsset href = do
  here <- asks e_uri
  case parseURIReference (escapeURIString isUnescapedInURI $ T.unpack href) of
    Just (flip relativeTo here -> uri) -> pure uri
    _ -> empty



-- hasGoogleAnalytics :: Ranker Bool
-- hasGoogleAnalytics = fmap or $ chroots "script" $ do
--   t <- text "script"
--   pure $ any (flip T.isInfixOf t)
--     [ "GoogleAnalyticsObject"
--     , "google-analytics"
--     , "googleAnalyticsCode"
--     , "UA-"
--     ]

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


-- hasSticky :: Ranker Bool
-- hasSticky = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "sticky"


--hasModal :: Ranker Bool
--hasModal = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "modal"
---- #email-popup-main-wrapper
---- https://www.mightynetworks.com/encyclopedia/passion-economy
----
---- div class="wppopups-whole"
---- <div id="newsletter-signup-modal">
---- <div role="dialog" aria-modal="true" id="newsletter-signup-modal-inner" tabindex="0" class="lv-modal newsletter-signup-modal lv-show" instance="popup" style="background-image: url(&quot;https://assetssc.leevalley.com:443/en-ca/-/media/images/01_homepage/feature-content-slider/09-2019/tools_joinery-project_01_fs.jpg?h=1111&amp;la=en-ca&amp;w=2880&amp;revision=6615dd6e-9bdc-4116-9ecc-b2e2dfd506b5&amp;modified=20190515203513&amp;hash=3FEF3022E01D02D167BF02376DEA6F527E5D5B7F&quot;);">


rankStats :: Ranker (PageStats Identity)
rankStats = PageStats
  <$> fmap fromIntegral jsBundleSize
  <*> fmap fromIntegral cssBundleSize
  <*> fmap fromIntegral tweets
  <*> fmap genericLength (chroots "img" gif)
  <*> pure False

