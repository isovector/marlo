{-# LANGUAGE AllowAmbiguousTypes #-}

module Search.Common where

import           API
import           Control.Monad (when)
import           DB (Connection)
import           Data.Int (Int64)
import           Data.Proxy
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Time (NominalDiffTime)
import qualified Lucid as L
import qualified Lucid.Servant as L
import           Search.Machinery
import           Search.Parser (encodeQuery)
import           Servant (toQueryParam)
import           Servant.API (SourceIO)
import           Servant.Server.Generic ()
import           Servant.StreamingUtil
import           Text.Printf (printf)
import           Types
import           Utils (commafy)


searchPage
    :: forall v
     . SearchMethod v
    => Connection
    -> Search Text
    -> NominalDiffTime
    -> PageNumber
    -> Int64
    -> SearchMethodResult v
    -> SourceIO (L.Html ())
searchPage conn q dur page cnt res = streamingToSourceT $ do
  yield $ do
    L.toHtmlRaw @Text "<html>"
    L.html_ $ do
      L.head_ $ do
        L.title_ $ mconcat
          [ "marlo search - results for "
          , L.toHtml (encodeQuery q)
          , " (" <> fromString (show cnt)
          , ")"
          ]
        L.link_ [L.rel_ "stylesheet", L.href_ "/common.css" ]
        L.link_ [L.rel_ "stylesheet", L.href_ "/results.css" ]
        L.script_ [L.type_ "text/javascript", L.src_ "size.js"] $ id @Text ""
  yield $ do
    L.toHtmlRaw @Text "<body>"
    L.toHtmlRaw @Text "<div class='box'>"
    searchBar (demote @v) $ Just q
    L.toHtmlRaw @String
      $ printf "%s results &mdash; search took %6.2fs seconds"
          (commafy $ show cnt)
          (realToFrac @_ @Double dur)
  showResults @v conn res
  yield $
    pager q (limitStrategy @v) (demote @v) cnt page


searchBar :: SearchVariety -> Maybe (Search Text) -> L.Html ()
searchBar v t =
  L.div_ [L.class_ "logo-box"] $ do
    L.form_ [ L.action_ "/search", L.method_ "GET" ] $ do
      L.h1_ "mar"
      L.input_ $
        [ L.id_ "query"
        , L.type_ "text"
        , L.name_ "q"
        , L.value_ $ maybe "" encodeQuery t
        ] <>
        [ L.autofocus_
        | t == Nothing
        ]
      L.select_ [ L.name_ "v" ] $ do
        L.option_ (selected Traditional [ L.value_ $ toQueryParam Traditional ]) "traditional"
        L.option_ (selected Spatial     [ L.value_ $ toQueryParam Spatial ])     "spatial"
  where
    selected v' z
      | v == v' = L.selected_ "selected" : z
      | otherwise = z


searchHref :: SearchVariety -> Search Text -> PageNumber -> L.Attribute
searchHref v q p =
  L.safeAbsHref_ (Proxy @API) (Proxy @SearchEndpoint) (Just v) (Just q) (Just $  p)


pager :: Search Text -> LimitStrategy -> SearchVariety -> Int64 -> PageNumber ->  L.Html ()
pager _ (Limit _) _ _ _ = pure ()
pager q (Paginate pagesize) v cnt page = do
  L.footer_ $ do
    when (page > 1) $ do
      L.a_ [ L.class_ "pager", searchHref v q $ page - 1 ] "Previous"
    when (fromIntegral ((getPageNumber page) * pagesize) < cnt) $ do
      L.a_ [ L.class_ "pager", searchHref v q $ page + 1 ] "Next"

