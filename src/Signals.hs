{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Signals (canonical) where

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


-- hasGoogleAnalytics :: Ranker Bool
-- hasGoogleAnalytics = fmap or $ chroots "script" $ do
--   t <- text "script"
--   pure $ any (flip T.isInfixOf t)
--     [ "GoogleAnalyticsObject"
--     , "google-analytics"
--     , "googleAnalyticsCode"
--     , "UA-"
--     ]

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

