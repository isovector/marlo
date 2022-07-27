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

import           Control.Applicative (liftA2)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           DB
import           Data.Bifunctor (first)
import           Data.Coerce (coerce)
import           Data.Either (partitionEithers)
import           Data.Foldable (for_, toList)
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Proxy
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Hasql.Connection (acquire, Connection)
import           Hasql.Session (run, statement)
import qualified Lucid as L
import           Network.URI (escapeURIString, isUnescapedInURI)
import           Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import qualified Network.Wai.Handler.Warp as W
import           Rel8 hiding (index)
import           Rel8.TextSearch
import           Search.Parser (searchParser)
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Servant.Server.Generic ()
import           Text.Megaparsec (parse, errorBundlePretty)
import           Types
import           Utils (paginate, timing)
import           WaiAppStatic.Types (MaxAge(NoMaxAge))
import Config


compileSearch :: Search Text -> Query (Discovery' Expr)
compileSearch q = orderBy ((\x -> rank (d_search x) (lit q') rDISTANCE) >$< desc) $
  case compile' q of
    Match ts -> matching ts
    Full qu -> qu
  where
    q' = compileQuery q


data IL
  = Match Tsquery
  | Full (Query (Discovery' Expr))

compile' :: Search Text -> IL
compile' (Term txt) = Match $ TsqTerm txt
compile' (Phrase []) = Match $ TsqTerm ""
compile' (Phrase txts) = Match $ foldr1 TsqPhrase $ fmap TsqTerm txts
compile' (Negate se) =
  case compile' se of
    Match ts -> Match $ TsqNot ts
    Full qu -> Full $ except (each discoverySchema') qu
compile' (And lhs rhs) = merge TsqAnd intersect (compile' lhs) (compile' rhs)
compile' (Or lhs rhs) = merge TsqOr union (compile' lhs) (compile' rhs)
compile' (SiteLike t) = Full $ do
  d <- each discoverySchema'
  where_ $ like (lit $ "%" <> t <> "%") $ d_uri $ d_table d
  pure d

merge
    :: (Tsquery -> Tsquery -> Tsquery)
    -> (Query (Discovery' Expr) -> Query (Discovery' Expr) -> Query (Discovery' Expr))
    -> IL
    -> IL
    -> IL
merge t _ (Match t1) (Match t2) = Match $ t t1 t2
merge _ q (Match t1) (Full q2) = Full $ q (matching t1) q2
merge _ q (Full q1) (Match t2) = Full $ q q1 (matching t2)
merge _ q (Full q1) (Full q2) = Full $ q q1 q2

matching :: Tsquery -> Query (Discovery' Expr)
matching q = do
  d <- each discoverySchema'
  where_ $ match (d_search d) $ lit q
  pure d


compileQuery :: Search Text -> Tsquery
compileQuery (Phrase []) = TsqTerm ""
compileQuery (Phrase wids) =
  foldr1 TsqPhrase $ fmap TsqTerm wids
compileQuery (Term wid) = TsqTerm wid
compileQuery (And lhs rhs) = TsqAnd (compileQuery lhs) (compileQuery rhs)
compileQuery (Or lhs rhs) = TsqOr (compileQuery lhs) (compileQuery rhs)
compileQuery (Negate lhs) = TsqNot (compileQuery lhs)
compileQuery (SiteLike _) = TsqTerm ""


getSnippet :: DocId -> Tsquery -> Query (Expr Text)
getSnippet did q = do
  d <- d_table <$> each discoverySchema'
  where_ $ d_docId d ==. lit did
  pure $ headline (d_content d <>. " " <>. d_headings d <>. " " <>. d_comments d) $ lit q


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
          $ getSnippet (d_docId $ d_table doc) $ compileQuery q
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
          for_ (zip (fmap d_table docs) snips) $ uncurry searchResult
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
main = runTestServer cfg_port

