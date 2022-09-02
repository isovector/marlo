{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Signals where

import           Assets (getAssetSizes)
import           Control.Applicative (optional, empty, liftA2, many, (<|>))
import           Control.Monad.Reader
import           DB
import           Data.Aeson (decode)
import           Data.ByteString.Lazy (fromStrict)
import           Data.Char (toLower, isAlpha, isDigit)
import           Data.Containers.ListUtils (nubOrdOn, nubOrd)
import           Data.Foldable (asum, fold)
import           Data.Int (Int64)
import           Data.List (isSuffixOf, partition, dropWhileEnd, isInfixOf, genericLength)
import           Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import           Data.SchemaOrg
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Traversable (for)
import           Keywords
import           Network.URI
import           Signals.Listicle (isListicle)
import           Text.HTML.Scalpel
import           Types
import           Utils
import Rel8.StateMask (BitMask (BitMask), flag, flagIf)
import Data.Bool (bool)


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
          , ".csv"
          , ".cdf"
          , ".mkv"
          , ".c"
          , ".txt"
          , ".bmp"
          , ".gz"
          , ".py"
          , ".xls"
          , ".json"
          , ".xlsl"
          , ".xlsx"
          , ".xz"
          , ".ppt"
          , ".ipyn"
          , ".ipynb"
          , ".asc"
          , ".xml"
          , ".rss"
          , ".svg"
          ]
      , not $ any (isOnDomain $ uriRegName auth) forbidSites
      , isWiki auth "wikipedia.org"
      , isWiki auth "wiktionary.org"
      , isWiki auth "wikimedia.org"
      , specificAllowRules uri
      ]
    | otherwise = False
  where
    path = fmap toLower $ uriPath uri

isWiki :: URIAuth -> String -> Bool
isWiki auth site =
  not (isOnDomain (uriRegName auth) site)
    || (isOnDomain (uriRegName auth) site
    && isOnDomain (uriRegName auth) ("en." <> site))


(==>) :: Bool -> Bool -> Bool
(==>) x y = not x || y

forbidSites :: [String]
forbidSites =
  [ "airbnb.com"
  , "aliexpress.com"
  , "amazon.ca"
  , "amazon.co.uk"
  , "amazon.com"
  , "amazon.de"
  , "anonfiles.com"
  , "archive.org"
  , "archive.today"
  , "bandcamp.com"
  , "hobbyking.com"
  , "bing.com"
  , "bitly.com"
  , "bloomberg.com"
  , "brid.gy"
  , "duckduckgo.com"
  , "ebay.com"
  , "etsy.com"
  , "facebook.com"
  , "flickr.com"
  , "forbes.com"
  , "ghostarchive.org"
  , "gist.github.com"
  , "gofile.io"
  , "goo.gl"
  , "goodreads.com"
  , "google.com"
  , "googleusercontent.com"
  , "gutenberg.org"
  , "imgur.com"
  , "imdb.com"
  , "buzzfeed.com"
  , "instagram.com"
  , "justpaste.it"
  , "last.fm"
  , "linkedin.com"
  , "live.com"
  , "mediafire.com"
  , "mega.nz"
  , "nitter.net"
  , "yandex.com"
  , "netflix.com"
  , "office.com"
  , "pastebin.com"
  , "peacocktv.com"
  , "pintrest.com"
  , "patreon.com"
  , "amzn.com"
  , "amzn.to"
  , "redbubble.com"
  , "reddit.com"
  , "scribd.com"
  , "snapchat.com"
  , "society6.com"
  , "spoilertv.com"
  , "spotify.com"
  , "t.co"
  , "tiktok.com"
  , "tinyurl.com"
  , "tikkun.org"
  , "metacritic.com"
  , "github.com"
  , "twitch.tv"
  , "tumblr.com"
  , "twitter.com"
  , "vimeo.com"
  , "whatsapp.org"
  , "wikidata.org"
  , "wikipedia.org"
  , "wp.me"
  , "yahoo.com"
  , "youtu.be"
  , "youtube.com"
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
  , "/topics/"
  , "/product/"
  , "/products/"
  , "/comment/"
  , "/opinion/"
  , "/opinions/"
  , "/newsletter-issue/"
  , "/feed/"
  , "/feeds/"
  , "/user/"
  , "/users/"
  , "/author/"
  , "/authors/"
  , "/news/"
  , "/commit/"
  , "/tree/"
  , "/blob/"
  , "/vod/"
  , "/watch/"
  , "/trending/"
  , "/politics/20"
  , "archive"
  , "/rss"
  , "/feed"
  , "/download"
  , "search-results"
  ]


-- | IMPORTANT
--
-- THIS IS ALL LOWER CASE
--
-- DO NOT PUT A CAPITAL IN YOUR PATH CHECKS
specificAllowRules :: URI -> Bool
specificAllowRules uri
  | Just auth <- uriAuthority uri =
    let on_domain = isOnDomain (uriRegName auth)
        is_wiki site = or
          [ on_domain site && isInfixOf "template:" path
          , on_domain site && isInfixOf "talk:" path
          , on_domain site && isInfixOf "category:" path
          , on_domain site && isInfixOf "special:" path
          , on_domain site && isInfixOf "template:" path
          ]
     in not $ or $
  -- Succeeds if none of the following are true
  fmap (flip isInfixOf path) forbidPaths <>
  [ on_domain "github.com" && isInfixOf "/commit/" path
  , on_domain "github.com" && isInfixOf "/commits/" path
  , on_domain "github.com" && isInfixOf "/blob/" path
  , on_domain "github.com" && isInfixOf "/edit/" path
  , on_domain "github.com" && isInfixOf "/stargazers" path
  , on_domain "github.com" && isInfixOf "/network/members" path
  , on_domain "github.com" && isInfixOf "/branches" path
  , on_domain "neocities.org" && isInfixOf "/site/" path
  , is_wiki "wikipedia.org"
  , is_wiki "wiktionary.org"
  , is_wiki "wikimedia.org"
  , isYearMonthPage path
  ]
  | otherwise = error "yo"
  where
    path = fmap toLower $ uriPath uri


isYearMonthPage :: String -> Bool
isYearMonthPage p =
  case takeEnd 2
      $ fmap T.unpack
      $ filter (not . T.null)
      $ T.split (== '/')
      $ T.pack p of
    [year@(y:_:_:_:[]), month@(_:_:[])]
      | all isDigit year
      , all isDigit month
      , elem y ['1', '2']
      -> True
    [_, year@(y:_:_:_:[])]
      | all isDigit year
      , elem y ['1', '2']
      -> True
    [year@(y:_:_:_:[])]
      | all isDigit year
      , elem y ['1', '2']
      -> True
    _ -> False


takeEnd :: Int -> [a] -> [a]
takeEnd n ls =
  let sz = length ls
   in drop (sz - n) ls


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


isOnDomain :: String -> String -> Bool
isOnDomain x dom = dom == x || isSuffixOf ('.' : dom) x


links :: Ranker [URI]
links = fmap nubOrd $ chroots "a" link


paraHeadingRatio :: Ranker (Int, Int)
paraHeadingRatio = do
  headings <- fmap join $ for [id @Int 1..5] $ \i -> chroots (tagSelector $ "h" <> show i) $ pure ()
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
  pure $ or
    [ T.isInfixOf "adsbygoogle" src
    , T.isInfixOf "pubads()" src
    ]


hasSticky :: Ranker Bool
hasSticky = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "sticky"

hasModal :: Ranker Bool
hasModal = fmap (not . null) $ chroots "div" $ withClass "div" $ T.isInfixOf "modal"
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

hasPaywall :: Ranker Bool
hasPaywall = do
  ds <- texts $ "script" @: ["type" @= "application/ld+json"]
  pure $ flip any ds $ \d ->
    case decode $ fromStrict $ encodeUtf8 d of
      Nothing -> False
      Just (IsAccessibleForFree b) -> not b


canBeFilteredOutBySchemaType :: Ranker (BitMask DocumentFlag)
canBeFilteredOutBySchemaType = do
  uri <- asks $ e_uri
  -- HACK: Substack and medium stupidly tag themselves as a news article
  let is_substack = maybe False (isInfixOf "substack.com" . uriRegName) $ uriAuthority uri
  let is_medium = maybe False (isInfixOf "medium.com" . uriRegName) $ uriAuthority uri

  ds <- texts $ "script" @: ["type" @= "application/ld+json"]
  pure $ flip foldMap ds $ \d ->
    case fmap getMetadataType $ decode $ fromStrict $ encodeUtf8 d of
      Just "AggregateOffer" -> flag IsShopping
      Just "AmpStory"       -> flag IsAnticompetitiveTech
      Just "Clip"           -> flag IsMedia
      Just "Episode"        -> flag IsMedia
      Just "Movie"          -> flag IsMedia
      Just "NewsArticle"    ->
        case is_substack || is_medium of
          True -> mempty
          False -> flag IsNews
      Just "Offer"          -> flag IsShopping
      Just "Product"        -> flag IsShopping
      Just "TVSeason"       -> flag IsMedia
      Just "TVSeries"       -> flag IsMedia
      _                     -> mempty


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

