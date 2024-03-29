{-# LANGUAGE NumDecimals #-}

module Search where

import           API
import           Config
import           Control.Monad.IO.Class (liftIO)
import           DB
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Linear.V3 (V3(V3))
import qualified Lucid as L
import           Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import qualified Network.Wai.Handler.Warp as W
import           Rel8 hiding (max, index)
import           ReverseSearch (reverseSearch)
import           Search.Common (searchBar, defaultSearchDims)
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
  (disc_size, exp_size, prune_size) <- liftIO $ do
    Right [exp_size] <-
      doSelect conn $ countRows $ do
        d <- each documentSchema
        where_ $ d_flags d ==. lit mempty
        pure ()
    Right [disc_size] <-
      doSelect conn $ countRows $ each discoverySchema
    Right [prune_size] <-
      doSelect conn $ countRows $ do
        d <- each documentSchema
        where_ $ d_flags d /=. lit mempty
        pure ()
    pure (disc_size, exp_size, prune_size)
  pure $
    L.html_ $ do
      L.head_ $ do
        L.link_ [L.rel_ "stylesheet", L.href_ "/common.css" ]
        L.link_ [L.rel_ "stylesheet", L.href_ "/style.css" ]
        L.script_ [L.type_ "text/javascript", L.src_ "size.js"] $ id @Text ""
        L.title_ "marlo: search, for humans"
      L.body_ $ L.div_ $ do
        searchBar Spatial defaultSearchDims Nothing
        L.div_ [L.class_ "metrics"] $ do
          L.span_ $ mconcat
            [ "Indexed: "
            , L.toHtml (commafy $ show exp_size)
            , " / Discovered: "
            , L.toHtml (commafy $ show disc_size)
            , " / Polution: "
            , L.toHtml (commafy $ show prune_size)
            ]



search
    :: Connection
    -> Maybe WindowSize
    -> Maybe SearchVariety
    -> Maybe (Search Text)
    -> Maybe PageNumber
    -> Maybe SearchDimension
    -> Maybe SearchDimension
    -> Maybe SearchDimension
    -> Handler (SourceIO (L.Html ()))
search _ _ _ Nothing _ _ _ _ =
  pure $ streamingToSourceT $ yield "Give me some keywords, punk!"
search conn ws
      (fromMaybe Traditional -> v)
      (Just q)
      mpage
      (fromMaybe ByWordCount -> x)
      (fromMaybe ByAssetSize -> y)
      (fromMaybe ByRelevance -> z) = do
  liftIO $ putStrLn $ mappend (show v) $ '/' : (T.unpack $ encodeQuery q)
  case toSing v of
    SomeSearchVariety (s :: SSearchVariety v) ->
      case dict1 @SearchMethod s of
        Dict1 ->
          doSearch @v conn (fromMaybe (WindowSize 1920 0) ws) (V3 x y z) q mpage


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

