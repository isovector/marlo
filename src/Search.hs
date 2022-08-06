{-# OPTIONS_GHC -Wno-orphans #-}

module Search where

import           Config
import           Control.Monad.IO.Class (liftIO)
import           DB
import           Data.Bifunctor (first)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Hasql.Connection (acquire, Connection)
import           Hasql.Session (run, statement)
import qualified Lucid as L
import           Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import qualified Network.Wai.Handler.Warp as W
import           Rel8 hiding (max, index)
import           Search.Parser (searchParser)
import           Search.SearchBar (searchBar)
import           Search.Spatial (spatialSearch)
import           Search.Traditional (traditionalSearch)
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Servant.Server.Generic ()
import           Text.Megaparsec (parse, errorBundlePretty)
import           Types
import           WaiAppStatic.Types (MaxAge(NoMaxAge))



home :: Connection -> Handler (L.Html ())
home conn = do
  sizes <- liftIO $ do
    Right sizes <- fmap (fmap M.fromList) $
      flip run conn $ statement () $ select $ do
        aggregate $ do
          d <- each discoverySchema
          pure (groupBy $ d_state d, countStar)
    pure sizes
  pure $
    L.html_ $ do
      L.head_ $ do
        L.link_ [L.rel_ "stylesheet", L.href_ "style.css" ]
        L.title_ "marlo: search, for humans"
      L.body_ $ L.div_ $ do
        searchBar Traditional ""
        L.div_ [L.class_ "metrics"] $ do
          L.span_ $ mconcat
            [ "Indexed: "
            , L.toHtml (commafy $ show $ fromMaybe 0 (M.lookup Explored sizes))
            , " / Discovered: "
            , L.toHtml (commafy $ show $ fromMaybe 0 (M.lookup Discovered sizes))
            ]


commafy :: String -> String
commafy
  = T.unpack
  . T.intercalate ","
  . reverse
  . fmap T.reverse
  . T.chunksOf 3
  . T.reverse
  . T.pack



search :: Connection -> Maybe SearchVariety -> Maybe (Search Text) -> Maybe Int -> Handler (L.Html ())
search _ _ Nothing _ = pure $ "Give me some keywords, punk!"
search conn (fromMaybe Traditional -> Traditional) (Just q) mpage = traditionalSearch conn q mpage
search conn (fromMaybe Traditional -> Spatial) (Just q) _ = spatialSearch conn q


------------------------------------------------------------------------------

type TestApi =
       Get '[HTML] (L.Html ())
  :<|> "search"
        :> QueryParam "v" SearchVariety
        :> QueryParam "q" (Search Text)
        :> QueryParam "p" Int
        :> Get '[HTML] (L.Html ())
  :<|> Raw


instance FromHttpApiData [Text] where
  parseQueryParam = Right . T.split (== ' ')

instance FromHttpApiData (Search Text) where
  parseQueryParam = first (T.pack . errorBundlePretty) . parse searchParser ""

instance FromHttpApiData SearchVariety where
  parseQueryParam "traditional" = Right Traditional
  parseQueryParam "spatial"     = Right Spatial
  parseQueryParam _             = Left "SearchVariety must be one of 'traditional' or 'spatial'"


server :: Connection -> Server TestApi
server conn = home conn
         :<|> search conn
         :<|> serveDirectoryWith (defaultWebAppSettings "static") { ssMaxAge = NoMaxAge }


runTestServer :: Connection -> W.Port -> IO ()
runTestServer conn port = W.run port $ serve (Proxy @TestApi) $ server conn


main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  runTestServer conn cfg_port

