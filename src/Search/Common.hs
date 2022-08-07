module Search.Common where

import           API
import           Data.Proxy
import           Data.Text (Text)
import qualified Lucid as L
import qualified Lucid.Servant as L
import           Servant.Server.Generic ()
import           Types
import Search.Parser (encodeQuery)
import Data.Int (Int64)
import Control.Monad (when)


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
        L.option_ (selected Traditional [ L.value_ "traditional" ]) "traditional"
        L.option_ (selected Spatial     [ L.value_ "spatial" ])     "spatial"
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

