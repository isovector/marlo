module Search.SearchBar where

import           Data.Text (Text)
import qualified Lucid as L
import           Servant.Server.Generic ()
import           Types


searchBar :: SearchVariety -> Text -> L.Html ()
searchBar v t =
  L.div_ [L.class_ "logo-box"] $ do
    L.form_ [ L.action_ "/search", L.method_ "GET" ] $ do
      L.h1_ "mar"
      L.input_ $
        [ L.id_ "query"
        , L.type_ "text"
        , L.name_ "q"
        , L.value_ t
        ] <>
        [ L.autofocus_
        | t == ""
        ]
      L.select_ [ L.name_ "v" ] $ do
        L.option_ (selected Traditional [ L.value_ "traditional" ]) "traditional"
        L.option_ (selected Spatial     [ L.value_ "spatial" ])     "spatial"
  where
    selected v' z
      | v == v' = L.selected_ "selected" : z
      | otherwise = z

