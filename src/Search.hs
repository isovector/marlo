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
import           Spider (getWordIds)
import           Text.Megaparsec (parse, errorBundlePretty)
import           Types
import           Utils (paginate, timing)
import           WaiAppStatic.Types (MaxAge(NoMaxAge))
import Network.URI (escapeURIString, isUnescapedInURI)



blah = do
  where_ $ true ==. false
  pure $ lit $ DocId 0

byDocId m = do
  d <- m
  doc <- each discoverySchema
  where_ $ d_docId doc ==. d
  pure doc { d_data = "" }

compileSearch :: Search WordId -> Query (Expr DocId)
compileSearch (Phrase []) = blah
compileSearch (Phrase wids@(w1 : wr)) = do
  distinct $ do
    i1 <- each indexSchema
    where_ $ i_wordId i1 ==. lit w1
    let p1 = i_position i1
    distinct $ foldr1 intersect $ do
      (ix, wid) <- zip [1..] wr
      pure $ do
        w <- each indexSchema
        where_ $ i_wordId w ==. lit wid &&. i_position w ==. p1 + lit ix &&. i_docId w ==. i_docId i1
        pure $ i_docId w
compileSearch (Term wid) =
  distinct $ do
    w <- each indexSchema
    where_ $ i_wordId w ==. lit wid
    pure $ i_docId w
compileSearch (Negate q) = do
  except (fmap d_docId $ each discoverySchema) $ compileSearch q
compileSearch (And q1 q2) = do
  intersect (compileSearch q1) $ compileSearch q2
compileSearch (Or q1 q2) = do
  union (compileSearch q1) $ compileSearch q2
compileSearch (SiteLike t) = do
  d <- each discoverySchema
  where_ $ like (lit $ "%" <> t <> "%") (d_uri d)
  pure $ d_docId d


getSnippet :: DocId -> [WordId] -> Query (Expr Bool, Expr Text)
getSnippet d [] = do
  where_ $ lit True ==. lit False
  pure $ lit (False, "")
getSnippet d ws@(w : _) = do
  i <- limit 1 $ do
      i <- each indexSchema
      where_ $ i_docId i ==. lit d &&. i_wordId i ==. lit w
      pure i
  let p = i_position i
  wid <-
    limit 10 $ fmap snd $ orderBy (fst >$< asc) $ do
      j <- each indexSchema
      where_ $ i_docId j ==. lit d &&. ( (-5 <=. i_position j - p &&. i_position j - p <=. 5)
                                       )
      pure (i_position j, i_wordId j)
  w <- each wordsSchema
  where_ $ w_wordId w ==. wid
  pure (in_ (w_wordId w) $ fmap lit ws , w_word w)


evaluateTerm :: Connection -> Search Keyword -> IO (Search WordId)
evaluateTerm conn q = do
  let kws = toList q
  Right wids <- flip run conn $ statement () $ select $ getWordIds kws
  let widmap =
        M.fromList $ do
          wid <- wids
          pure (Keyword $ w_word wid, w_wordId wid)
  pure $ fmap (widmap M.!) q


-- API specification
type TestApi =
       Get '[HTML] (L.Html ())
  :<|> "search"
        :> QueryParam "q" (Search Keyword)
        :> QueryParam "p" Int
        :> Get '[HTML] (L.Html ())
  :<|> Raw


instance FromHttpApiData [Keyword] where
  parseQueryParam = Right . fmap Keyword . T.split (== ' ')

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


search :: Maybe (Search Keyword) -> Maybe Int -> Handler (L.Html ())
search Nothing _ = pure $ "Give me some keywords, punk!"
search (Just q) mpage = do
  let pagenum = Prelude.max 0 $ maybe 0 (subtract 1) mpage
      pagesize :: Num a => a
      pagesize = 20
  (cnt, docs, snips) <- liftIO $ do
    Right conn <- acquire connectionSettings
    swid <- timing "lookup keywords" $ evaluateTerm conn q
    writeFile "/tmp/lastquery.sql" $ showQuery $ byDocId $ compileSearch swid
    Right (cnt, docs) <- timing "find documents" $ fmap (fmap unzip) $
      flip run conn
        $ statement ()
        $ select
        $ paginate pagesize (fromIntegral pagenum)
        $ let x = orderBy (d_rank >$< desc) $ byDocId $ compileSearch swid
           in liftA2 (,) (countRows x) x
    (_, snips) <- timing "building snippets" $ fmap partitionEithers $
      for docs $ \doc -> do
        putStrLn $ "building snippet for " <> show (d_docId doc)
        flip run conn
          $ statement ()
          $ select
          $ getSnippet (d_docId doc) $ toList swid
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

escape = T.pack . escapeURIString isUnescapedInURI . T.unpack

encodeQuery :: Search Keyword -> Text
encodeQuery (Term kw) = coerce kw
encodeQuery (Phrase keys) = "\"" <> (T.intercalate " " $ coerce keys) <> "\""
encodeQuery (Negate q) = "-(" <> encodeQuery q <> ")"
encodeQuery (And q1 q2) = encodeQuery q1 <> " " <> encodeQuery q2
encodeQuery (Or q1 q2) = "(" <> encodeQuery q1 <> ") OR (" <> encodeQuery q2 <> ")"
encodeQuery (SiteLike t) = "site:" <> t

searchResult :: Discovery Rel8.Result -> [(Bool, Text)] -> L.Html ()
searchResult d snip =
  L.div_ [L.class_ "result"] $ do
    L.span_ [L.class_ "url"] $ L.a_ [L.href_ $ d_uri d] $ L.toHtml $ d_uri d
    L.span_ [L.class_ "title"] $ L.a_ [L.href_ $ d_uri d] $ L.toHtml title
    L.p_ [L.class_ "snippet"] $ foldMap (uncurry boldify) snip
  where
    title =
      case T.strip $ d_title d of
        "" -> "(no title)"
        x -> x


boldify :: Bool -> Text -> L.Html ()
boldify False t = L.toHtml $ t <> " "
boldify True t = L.strong_ $ L.toHtml $ t <> " "



server :: Server TestApi
server = pure home :<|> search :<|> serveDirectoryWith (defaultWebAppSettings "static") { ssMaxAge = NoMaxAge }

instance FromHttpApiData (Search Keyword) where
  parseQueryParam = first (T.pack . errorBundlePretty) . parse searchParser ""

runTestServer :: W.Port -> IO ()
runTestServer port = W.run port $ serve (Proxy @TestApi) server

main :: IO ()
main = runTestServer 8001

