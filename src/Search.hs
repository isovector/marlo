{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Search where

import           Config
import           Control.Applicative (liftA2)
import           Control.Monad.IO.Class (liftIO)
import           DB
import           Data.Bifunctor (first)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Proxy
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Lucid as L
import           Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import qualified Network.Wai.Handler.Warp as W
import           Rel8 hiding (max, index)
import           Search.Compiler (compileSearch, encodeQuery)
import           Search.Machinery
import           Search.Parser (searchParser)
import           Search.SearchBar (searchBar)
import           Search.Spatial ()
import           Search.Traditional ()
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Servant.Server.Generic ()
import           Text.Megaparsec (parse, errorBundlePretty)
import           Types
import           Utils (timing)
import           WaiAppStatic.Types (MaxAge(NoMaxAge))



home :: Connection -> Handler (L.Html ())
home conn = do
  sizes <- liftIO $ do
    Right sizes <- fmap (fmap M.fromList) $
      doSelect conn $ do
        aggregate $ do
          d <- each documentSchema
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


doSearch :: forall v. SearchMethod v => Connection -> Search Text -> Maybe Int -> Handler (L.Html ())
doSearch conn q mpage = do
  let pagenum = fromIntegral $ Prelude.max 0 $ maybe 0 (subtract 1) mpage
  (cnt, res) <- liftIO $ do
    Right prepped <- timing "find documents" $ do
      doSelect conn $ do
        let x = compileSearch q
        prepareSearch @v pagenum $ liftA2 (,) (countRows x) x
    let cnt = maybe 0 fst $ listToMaybe prepped
        docs = fmap snd prepped
    res <- accumResults @v conn q docs
    pure (cnt, res)
  pure $
    L.html_ $ do
      L.head_ $ do
        L.title_ $ mconcat
          [ "marlo search - results for "
          , L.toHtml (encodeQuery q)
          , " (" <> fromString (show cnt)
          , ")"
          ]
        L.link_ [L.rel_ "stylesheet", L.href_ "results.css" ]
      L.body_ $ do
        L.div_ [L.class_ "box"] $ do
          searchBar (demote @v) $ encodeQuery q
          showResults @v q cnt pagenum res


search :: Connection -> Maybe SearchVariety -> Maybe (Search Text) -> Maybe Int -> Handler (L.Html ())
search _ _ Nothing _ = pure $ "Give me some keywords, punk!"
search conn (fromMaybe Traditional -> v) (Just q) mpage =
  case toSing v of
    SomeSearchVariety (s :: SSearchVariety v) ->
      case dict1 @SearchMethod s of
        Dict1 ->
          doSearch @v conn q mpage


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
  Right conn <- connect
  runTestServer conn cfg_port

