{-# LANGUAGE AllowAmbiguousTypes #-}

module Search.Common where

import           API
import           Control.Monad (when)
import           DB (Connection)
import           Data.Bool (bool)
import           Data.Foldable (for_)
import           Data.Proxy
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Time (NominalDiffTime)
import           Linear (V3 (V3))
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


bracketHtml :: Monad m => Text -> Text -> Streaming (L.Html ()) m a -> Streaming (L.Html ()) m a
bracketHtml start end =
  bracket (L.toHtmlRaw start) (L.toHtmlRaw end)


searchPage
    :: forall v
     . SearchMethod v
    => Connection
    -> V3 SearchDimension
    -> Search Text
    -> NominalDiffTime
    -> PageNumber
    -> Int64
    -> SearchMethodResult v
    -> SourceIO (L.Html ())
searchPage conn dims q dur page cnt res = streamingToSourceT $ do
  bracketHtml "<html>" "</html>" $ do
    yield $ do
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

    bracketHtml "<body>" "</body>" $
      bracketHtml "<div class='box'>" "</div>" $ do
        yield $ do
          searchBar (demote @v) dims $ Just q
          L.toHtmlRaw @String
            $ printf "%s results &mdash; search took %6.2fs seconds"
                (commafy $ show cnt)
                (realToFrac @_ @Double dur)
        showResults @v conn q res
        yield $ pager q (limitStrategy @v) (demote @v) cnt page


searchBar :: SearchVariety -> V3 SearchDimension -> Maybe (Search Text) -> L.Html ()
searchBar v (V3 x y z) t =
  L.div_ [L.class_ "logo-box"] $ do
    L.form_ [ L.action_ "/discover", L.method_ "GET" ] $ do
      L.div_ [L.id_ "searchbar"] $ do
        L.input_ $
          [ L.id_ "query"
          , L.type_ "text"
          , L.name_ "q"
          , L.value_ $ maybe "" encodeQuery t
          ] <>
          [ L.autofocus_
          | t == Nothing
          ]
        L.h1_ "mar"
      L.div_ [L.id_ "searchopts"] $ do
        L.label_ $ do
          "style: "
          L.select_ [ L.name_ "v" ] $ do
            L.option_ (selected Traditional [ L.value_ $ toQueryParam Traditional ]) "traditional"
            L.option_ (selected Spatial     [ L.value_ $ toQueryParam Spatial ])     "spatial"
        buildDim "x" x
        buildDim "y" y
        buildDim "z" z

        L.input_ [ L.id_ "go", L.type_ "submit", L.value_ "discover »" ]
  where
    buildDim :: Text -> SearchDimension -> L.Html ()
    buildDim nm a = L.label_ $ do
      L.toHtml $ nm <> ": "
      L.select_ [L.name_ nm] $
        for_ (enumFrom @SearchDimension minBound) $ \d ->
            L.option_
                (L.value_ (toQueryParam d) : bool [] (pure $ L.selected_ "selected") (d == a))
              $ L.toHtml
              $ toQueryParam d


    selected v' a
      | v == v' = L.selected_ "selected" : a
      | otherwise = a


defaultSearchDims :: V3 SearchDimension
defaultSearchDims = V3 ByWordCount ByAssetSize ByRelevance

searchHref :: SearchVariety -> Search Text -> PageNumber -> L.Attribute
searchHref v q p =
  L.safeAbsHref_ (Proxy @API) (Proxy @SearchEndpoint) (Just v) (Just q) (Just p) Nothing Nothing Nothing


pager :: Search Text -> LimitStrategy -> SearchVariety -> Int64 -> PageNumber ->  L.Html ()
pager _ (Limit _) _ _ _ = pure ()
pager q (Paginate pagesize) v cnt page = do
  L.footer_ $ do
    when (page > 1) $ do
      L.a_ [ L.class_ "pager", searchHref v q $ page - 1 ] "Previous"
    when (fromIntegral ((getPageNumber page) * pagesize) < cnt) $ do
      L.a_ [ L.class_ "pager", searchHref v q $ page + 1 ] "Next"

