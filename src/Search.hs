{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Search where

import qualified Data.Map as M
import           Control.Applicative (liftA2)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           DB
import           Data.Bifunctor (first)
import           Data.Coerce (coerce)
import           Data.Either (partitionEithers)
import           Data.Foldable (for_, toList)
import           Data.Functor.Contravariant ((>$<))
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Proxy
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Hasql.Connection (acquire, Connection)
import           Hasql.Session (run, statement)
import qualified Lucid as L
import           Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import qualified Network.Wai.Handler.Warp as W
import           Rel8 hiding (index)
import           Search.Parser (searchParser)
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Servant.Server.Generic ()
import           Text.Megaparsec (parse, errorBundlePretty)
import           Types
import           Utils (paginate, timing)
import           WaiAppStatic.Types (MaxAge(NoMaxAge))
import Network.URI (escapeURIString, isUnescapedInURI)
import Rel8.TextSearch

compileSearch :: Search Text -> Query (Discovery Expr)
compileSearch q = do
  d <- each discoverySchema
  where_ $ match (d_search d) $ lit $ compileQuery q
  pure d


compileQuery :: Search Text -> Tsquery
compileQuery (Phrase []) = error "nope"
compileQuery (Phrase wids) =
  foldr1 TsqPhrase $ fmap TsqTerm wids
compileQuery (Term wid) = TsqTerm wid
compileQuery (And lhs rhs) = TsqAnd (compileQuery lhs) (compileQuery rhs)
compileQuery (Or lhs rhs) = TsqOr (compileQuery lhs) (compileQuery rhs)
compileQuery (Negate lhs) = TsqNot (compileQuery lhs)
compileQuery (SiteLike _) = error "cant do sitelike yet"



getSnippet :: DocId -> Tsquery -> Query (Expr Text)
getSnippet did q = do
  d <- each discoverySchema
  where_ $ d_docId d ==. lit did
  pure $ headline (d_content d) $ lit q


type TestApi =
       Get '[HTML] (L.Html ())
  :<|> "search"
        :> QueryParam "q" (Search Text)
        :> QueryParam "p" Int
        :> Get '[HTML] (L.Html ())
  :<|> Raw


instance FromHttpApiData [Text] where
  parseQueryParam = Right . T.split (== ' ')


home :: L.Html ()
home =
  L.html_ $ do
    L.head_ $ do
      L.link_ [L.rel_ "stylesheet", L.href_ "style.css" ]
      L.title_ "marlo search"
    L.body_ $ searchBar ""


searchBar :: Text -> L.Html ()
searchBar t =
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


search :: Maybe (Search Text) -> Maybe Int -> Handler (L.Html ())
search Nothing _ = pure $ "Give me some keywords, punk!"
search (Just q) mpage = do
  let pagenum = Prelude.max 0 $ maybe 0 (subtract 1) mpage
      pagesize :: Num a => a
      pagesize = 20
  (cnt, docs, snips) <- liftIO $ do
    Right conn <- acquire connectionSettings
    writeFile "/tmp/lastquery.sql" $ showQuery $ compileSearch q
    Right (cnt, docs) <- timing "find documents" $ fmap (fmap unzip) $
      flip run conn
        $ statement ()
        $ select
        $ paginate pagesize (fromIntegral pagenum)
        $ let x = compileSearch q
           in liftA2 (,) (countRows x) x
    snips <- timing "building snippets" $
      for docs $ \doc -> do
        Right [snip] <- flip run conn
          $ statement ()
          $ select
          $ getSnippet (d_docId doc) $ compileQuery q
        pure snip
    pure (fromMaybe 0 (listToMaybe cnt), docs, snips)
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
          searchBar $ encodeQuery q
          for_ (zip docs snips) $ uncurry searchResult
          let eq = escape $ encodeQuery q
          when (pagenum > 0) $ do
            L.a_ [L.href_ $ "/search?q=" <> eq <> "&p=" <> T.pack (show pagenum)  ] "Prev"
          when ((pagenum + 1) * pagesize < fromIntegral cnt) $ do
            L.a_ [L.href_ $ "/search?q=" <> eq <> "&p=" <> T.pack (show (pagenum + 2))  ] "Next"
  where
    escape = T.pack . escapeURIString isUnescapedInURI . T.unpack

encodeQuery :: Search Text -> Text
encodeQuery (Term kw) = coerce kw
encodeQuery (Phrase keys) = "\"" <> (T.intercalate " " $ coerce keys) <> "\""
encodeQuery (Negate q) = "-(" <> encodeQuery q <> ")"
encodeQuery (And q1 q2) = encodeQuery q1 <> " " <> encodeQuery q2
encodeQuery (Or q1 q2) = "(" <> encodeQuery q1 <> ") OR (" <> encodeQuery q2 <> ")"
encodeQuery (SiteLike t) = "site:" <> t

searchResult :: Discovery Rel8.Result -> Text -> L.Html ()
searchResult d snip =
  L.div_ [L.class_ "result"] $ do
    L.span_ [L.class_ "url"] $ L.a_ [L.href_ $ d_uri d] $ L.toHtml $ d_uri d
    L.span_ [L.class_ "title"] $ L.a_ [L.href_ $ d_uri d] $ L.toHtml title
    L.p_ [L.class_ "snippet"] $ L.toHtmlRaw snip
  where
    title =
      case T.strip $ d_title d of
        "" -> "(no title)"
        x -> x


server :: Server TestApi
server = pure home :<|> search :<|> serveDirectoryWith (defaultWebAppSettings "static") { ssMaxAge = NoMaxAge }

instance FromHttpApiData (Search Text) where
  parseQueryParam = first (T.pack . errorBundlePretty) . parse searchParser ""

runTestServer :: W.Port -> IO ()
runTestServer port = W.run port $ serve (Proxy @TestApi) server

main :: IO ()
main = runTestServer 8001

