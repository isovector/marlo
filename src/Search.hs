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

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import           Servant
import           Servant.Server.Generic ()
import           Control.Applicative (liftA2, liftA3)
import           Control.Exception.Base
import           Control.Monad (forever, when, void)
import           DB
import           Data.Bifunctor (first, second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (for_, toList)
import           Data.Functor ((<&>))
import           Data.Functor.Contravariant ((>$<), (>$))
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int32)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (toStrict)
import           Data.Traversable (for)
import           Hasql.Connection (acquire, Connection)
import           Hasql.Session (run, statement)
import           Keywords (posWords)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import           Network.HTTP.Types (hContentType)
import           Network.URI (parseURI, URI)
import           Ranking (rank)
import qualified Rel8 as R8
import           Rel8 hiding (index)
import           Signals
import           Spider (getWordIds)
import           Types
import           Utils (runRanker, unsafeURI, paginate)
import Servant.HTML.Lucid (HTML)
import qualified Lucid as L
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Data.Maybe (fromMaybe, listToMaybe)
import Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import WaiAppStatic.Types (MaxAge(NoMaxAge))
import Data.Coerce (coerce)
import Data.Either (partitionEithers)



data Search a
  = Terms [a]
  | Phrase [a]
  -- -- | Or Search Search
  -- -- | Not Keyword
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

blah = do
  where_ $ true ==. false
  pure $ lit $ Discovery (DocId 0) "" Discovered 0 "" 0 ""

byDocId m = do
  d <- m
  doc <- each discoverySchema
  where_ $ d_docId doc ==. d
  pure doc

compileSearch :: Search WordId -> Query (Discovery Expr)
compileSearch (Terms []) = blah
compileSearch (Phrase []) = blah
compileSearch (Phrase wids@(w1 : wr)) = do
  byDocId $ distinct $ do
    i1 <- each indexSchema
    where_ $ i_wordId i1 ==. lit w1
    let p1 = i_position i1
    distinct $ foldr1 intersect $ do
      (ix, wid) <- zip [1..] wr
      pure $ do
        w <- each indexSchema
        where_ $ i_wordId w ==. lit wid &&. i_position w ==. p1 + lit ix &&. i_docId w ==. i_docId i1
        pure $ i_docId w
compileSearch (Terms wids) =
  byDocId $
    distinct $ foldr1 intersect $ do
      wid <- wids
      pure $ do
        w <- each indexSchema
        where_ $ i_wordId w ==. lit wid
        pure $ i_docId w


getSnippet :: DocId -> [WordId] -> Query (Expr Text)
getSnippet d [] = do
  where_ $ lit True ==. lit False
  pure $ lit ""
getSnippet d (w : ws) = fmap snd $ orderBy (fst >$< asc) $ do
  i <- each indexSchema
  where_ $ i_docId i ==. lit d &&. i_wordId i ==. lit w
  let p = i_position i
  j <- each indexSchema
  where_ $ i_docId i ==. lit d &&. abs (i_position j - p) <=. 5
  w <- each wordsSchema
  where_ $ w_wordId w ==. i_wordId j
  pure (i_position j, w_word w)






evaluateTerm :: Connection -> Search Keyword -> IO (Search WordId)
evaluateTerm conn (Terms kws) = do
  Right wids <- flip run conn $ statement () $ select $ getWordIds kws
  let not_in_corpus = S.fromList kws S.\\ S.fromList (fmap (Keyword . w_word) wids)
  print not_in_corpus
  pure $ Terms $ fmap w_wordId wids
evaluateTerm conn (Phrase kws) = do
  Right wids <- flip run conn $ statement () $ select $ getWordIds kws
  let not_in_corpus = S.fromList kws S.\\ S.fromList (fmap (Keyword . w_word) wids)
  print not_in_corpus
  pure $ Phrase $ fmap w_wordId wids



-- API specification
type TestApi =
       Get '[HTML] (L.Html ())
  :<|> "search"
        :> QueryParam "q" [Keyword]
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
      L.title_ "Yo"
    L.body_ $
      L.form_ [ L.action_ "/search", L.method_ "GET" ] $ do
        L.input_ [ L.id_ "query", L.type_ "text", L.name_ "q" ]
        -- L.input_ [ L.id_ "go", L.type_ "submit", L.value_ "Search!" ]


search :: Maybe [Keyword] -> Maybe Int -> Handler (L.Html ())
search Nothing _ = pure $ "Give me some keywords, punk!"
search (Just kws) mpage = do
  let q = Terms kws

      pagenum = Prelude.max 0 $ maybe 0 (subtract 1) mpage
      pagesize :: Num a => a
      pagesize = 20
  (cnt, docs, snips) <- liftIO $ do
    Right conn <- acquire connectionSettings
    swid <- evaluateTerm conn q
    Right (cnt, docs) <- fmap (fmap unzip) $
      flip run conn
        $ statement ()
        $ select
        $ paginate pagesize (fromIntegral pagenum)
        $ let x = orderBy (d_rank >$< desc) $ compileSearch swid
           in liftA2 (,) (countRows x) x
    for_ docs $ \doc ->
      putStrLn $ showQuery $ getSnippet (d_docId doc) $ toList swid
    -- (_, snips) <- fmap partitionEithers $
    --   for docs $ \doc ->
    --     flip run conn
    --       $ statement ()
    --       $ select
    --       $ getSnippet (d_docId doc) $ toList swid
    let snips = repeat ["Lorem ipsum"]
    pure (fromMaybe 0 (listToMaybe cnt), docs, snips)
  pure $
    L.html_ $ do
      L.head_ $ do
        L.title_ $ "Search Results for " <> fromString (show kws) <> " (" <> fromString (show cnt) <> ")"
        L.link_ [L.rel_ "stylesheet", L.href_ "results.css" ]
      L.body_ $ do
        for_ (zip docs snips) $ uncurry searchResult
        when (pagenum > 0) $ do
          L.a_ [L.href_ $ "/search?q=" <> encodeQuery q <> "&p=" <> T.pack (show pagenum)  ] "Prev"
        when ((pagenum + 1) * pagesize < fromIntegral cnt) $ do
          L.a_ [L.href_ $ "/search?q=" <> encodeQuery q <> "&p=" <> T.pack (show (pagenum + 2))  ] "Next"

encodeQuery :: Search Keyword -> Text
encodeQuery (Terms keys) = T.intercalate "+" $ coerce keys
encodeQuery (Phrase keys) = "\"" <> (T.intercalate "+" $ coerce keys) <> "\""

searchResult :: Discovery Rel8.Result -> [Text] -> L.Html ()
searchResult d snip =
  L.div_ [L.class_ "result"] $ do
    L.span_ [L.class_ "url"] $ L.a_ [L.href_ $ d_uri d] $ fromString $ T.unpack $ d_uri d
    L.br_ []
    L.span_ [L.class_ "title"] $ L.a_ [L.href_ $ d_uri d] $ fromString $ T.unpack $ d_title d
    L.p_ [L.class_ "snippet"] $ L.toHtml $ T.intercalate " " snip

server :: Server TestApi
server = pure home :<|> search :<|> serveDirectoryWith (defaultWebAppSettings "static") { ssMaxAge = NoMaxAge }

runTestServer :: W.Port -> IO ()
runTestServer port = W.run port $ serve (Proxy @TestApi) server

main :: IO ()
main = runTestServer 8001

