module Signals.AcceptableURI where

import           Data.Char (toLower, isDigit)
import           Data.List (isSuffixOf, isInfixOf)
import qualified Data.Text as T
import           Network.URI hiding (path)


isAcceptableLink :: URI -> Bool
isAcceptableLink uri
  | Just auth <- uriAuthority uri = and
      [ any (== uriScheme uri) ["http:", "https:"]
      , not $ any (`isSuffixOf` path)
          [ ".pdf"
          , ".png"
          , ".gif"
          , ".js"
          , ".c"
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
          , ".m4v"
          , ".iso"
          , ".graph"
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
  , "ehealthme.com"
  , "tvtropes.org"
  , "bing.com"
  , "bitly.com"
  , "bloomberg.com"
  , "brid.gy"
  , "clickhole.com"
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
  , "zoom.us"
  , "thesaurus.com"
  , "dictionary.com"
  , "abc7news.com"
  ]
  -- thesaurus
  -- dictionary


forbidPaths :: [String]
forbidPaths =
  [ "/tag/"
  , "/tags/"
  , "comment-page-"
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
  , "/columnists/"
  , "/people/"
  , "/authors/"
  , "/member/"
  , "/members/"
  , "/profiles/"
  , "/news/"
  , "/commit/"
  , "/solution/"
  , "/solutions/"
  , "/tree/"
  , "/blob/"
  , "/vod/"
  , "/watch/"
  , "/video/"
  , "/videos/"
  , "/trending/"
  , "/politics/20"
  , "archive"
  , "/rss"
  , "/feed"
  , "/download"
  , "search-results"
  , "project:"
  , "portal:"
  , "category:"
  , "special:"
  , "template:"
  , "talk:"
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


isOnDomain :: String -> String -> Bool
isOnDomain x dom = dom == x || isSuffixOf ('.' : dom) x


takeEnd :: Int -> [a] -> [a]
takeEnd n ls =
  let sz = length ls
   in drop (sz - n) ls

