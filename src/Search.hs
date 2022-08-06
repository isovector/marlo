{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Search where

import           API
import           Config
import           Control.Applicative (liftA2)
import           Control.Monad.IO.Class (liftIO)
import           DB
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
import           Search.Common (searchBar, pager)
import           Search.Compiler (compileSearch)
import           Search.Machinery
import           Search.Parser (encodeQuery)
import           Search.Spatial ()
import           Search.Traditional ()
import           Servant
import           Servant.Server.Generic ()
import           Text.Printf (printf)
import           Types
import           Utils (paginate, timeItT)
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
        searchBar Traditional Nothing
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


doSearch
    :: forall v
     . SearchMethod v
    => Connection
    -> Search Text
    -> Maybe PageNumber
    -> Handler (L.Html ())
doSearch conn q mpage = do
  let page = fromMaybe 1 mpage
      pagenum = subtract 1 $ Prelude.max 1 $ maybe 1 getPageNumber mpage
  (dur, (cnt, res)) <- liftIO $ timeItT $ do
    Right prepped <- do
      doSelect conn $ do
        let x = compileSearch q
        ( case limitStrategy @v of
            Limit n       -> limit n
            Paginate size -> paginate size pagenum
          ) $ liftA2 (,) (countRows x) x

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
          searchBar (demote @v) $ Just q
          L.toHtml @String
            $ printf "Search took: %6.4fs"
            $ realToFrac @_ @Double dur
          showResults @v res
          pager q (limitStrategy @v) (demote @v) cnt page


search :: Connection -> Maybe SearchVariety -> Maybe (Search Text) -> Maybe PageNumber -> Handler (L.Html ())
search _ _ Nothing _ = pure $ "Give me some keywords, punk!"
search conn v (Just q) mpage =
  case toSing $ fromMaybe Traditional v of
    SomeSearchVariety (s :: SSearchVariety v) ->
      case dict1 @SearchMethod s of
        Dict1 ->
          doSearch @v conn q mpage



------------------------------------------------------------------------------

server :: Connection -> Server API
server conn = home conn
         :<|> search conn
         :<|> serveDirectoryWith (defaultWebAppSettings "static") { ssMaxAge = NoMaxAge }


runTestServer :: Connection -> W.Port -> IO ()
runTestServer conn port = W.run port $ serve (Proxy @API) $ server conn


main :: IO ()
main = do
  Right conn <- connect
  runTestServer conn cfg_port

