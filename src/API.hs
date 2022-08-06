{-# OPTIONS_GHC -Wno-orphans #-}

module API where

import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Lucid as L
import           Search.Parser (searchParser)
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Servant.Server.Generic ()
import           Text.Megaparsec (parse, errorBundlePretty)
import           Types


type API =
       Get '[HTML] (L.Html ())
  :<|> SearchEndpoint
  :<|> Raw


type SearchEndpoint =
  "search"
    :> QueryParam "v" SearchVariety
    :> QueryParam "q" (Search Text)
    :> QueryParam "p" Int
    :> Get '[HTML] (L.Html ())


instance FromHttpApiData (Search Text) where
  parseQueryParam = first (T.pack . errorBundlePretty) . parse searchParser ""

