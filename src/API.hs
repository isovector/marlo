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
  :<|> "reverse" :> CaptureAll "uri" Text :> Get '[HTML] (L.Html ())
  :<|> SearchEndpoint
  :<|> Raw


type SearchEndpoint =
  "discover"
    :> Header "Cookie" WindowSize
    :> QueryParam "v" SearchVariety
    :> QueryParam "q" (Search Text)
    :> QueryParam "p" PageNumber
    :> QueryParam "x" SearchDimension
    :> QueryParam "y" SearchDimension
    :> QueryParam "z" SearchDimension
    :> StreamGet NewlineFraming HTML (SourceIO (L.Html ()))


instance FromHttpApiData (Search Text) where
  parseQueryParam = first (T.pack . errorBundlePretty) . parse searchParser ""

