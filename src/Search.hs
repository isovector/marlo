{-# LANGUAGE NumDecimals #-}

module Search where

import           API
import           Config
import           Control.Monad.IO.Class (liftIO)
import           DB
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Lucid as L
import           Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import qualified Network.Wai.Handler.Warp as W
import           Rel8 hiding (max, index)
import           ReverseSearch (reverseSearch)
import           Search.Common (searchBar)
import           Search.DoSearch (doSearch)
import           Search.Machinery
import           Search.Parser (encodeQuery)
import           Search.Spatial ()
import           Search.Traditional ()
import           Servant
import           Servant.Server.Generic ()
import           Servant.StreamingUtil
import           Types
import           Utils (commafy)
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
        L.link_ [L.rel_ "stylesheet", L.href_ "/common.css" ]
        L.link_ [L.rel_ "stylesheet", L.href_ "/style.css" ]
        L.script_ [L.type_ "text/javascript", L.src_ "size.js"] $ id @Text ""
        L.title_ "marlo: search, for humans"
      L.body_ $ L.div_ $ do
        searchBar Spatial Nothing
        L.div_ [L.class_ "metrics"] $ do
          L.span_ $ mconcat
            [ "Indexed: "
            , L.toHtml (commafy $ show $ fromMaybe 0 (M.lookup Explored sizes))
            , " / Discovered: "
            , L.toHtml (commafy $ show $ fromMaybe 0 (M.lookup Discovered sizes))
            ]


search
    :: Connection
    -> Maybe WindowSize
    -> Maybe SearchVariety
    -> Maybe (Search Text)
    -> Maybe PageNumber
    -> Handler (SourceIO (L.Html ()))
search _ _ _ Nothing _ =
  pure $ streamingToSourceT $ yield "Give me some keywords, punk!"
search conn ws (fromMaybe Traditional -> v) (Just q) mpage = do
  liftIO $ putStrLn $ mappend (show v) $ '/' : (T.unpack $ encodeQuery q)
  case toSing v of
    SomeSearchVariety (s :: SSearchVariety v) ->
      case dict1 @SearchMethod s of
        Dict1 ->
          doSearch @v conn (fromMaybe (WindowSize 1920 0) ws) q mpage


------------------------------------------------------------------------------

server :: Connection -> Server API
server conn = home conn
         :<|> reverseSearch conn
         :<|> search conn
         :<|> serveDirectoryWith (defaultWebAppSettings "static") { ssMaxAge = NoMaxAge }


runTestServer :: Connection -> W.Port -> IO ()
runTestServer conn port = W.run port $ serve (Proxy @API) $ server conn


main :: IO ()
main = do
  Right conn <- connect
  runTestServer conn cfg_port

