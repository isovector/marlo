{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Data.Foldable (for_)
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
import           Utils (runRanker, unsafeURI)
import Servant.HTML.Lucid (HTML)
import qualified Lucid as L
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Data.Maybe (fromMaybe)



data Search a
  = Terms [a]
  -- -- | Or Search Search
  -- -- | Not Keyword
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

compileSearch :: Search WordId -> Query (Discovery Expr)
compileSearch (Terms []) = do
  where_ $ true ==. false
  pure $ lit $ Discovery (DocId 0) "" Discovered 0 "" 0 ""
compileSearch (Terms wids) = distinct $ do
  d <- distinct $ foldr1 intersect $ do
    wid <- wids
    pure $ do
      w <- each indexSchema
      where_ $ i_wordId w ==. lit wid
      pure $ i_docId w
  doc <- each discoverySchema
  where_ $ d_docId doc ==. d
  pure doc


evaluateTerm :: Connection -> Search Keyword -> IO (Search WordId)
evaluateTerm conn (Terms kws) = do
  Right wids <- flip run conn $ statement () $ select $ getWordIds kws
  let not_in_corpus = S.fromList kws S.\\ S.fromList (fmap (Keyword . w_word) wids)
  print not_in_corpus
  pure $ Terms $ fmap w_wordId wids



-- API specification
type TestApi =
       Get '[HTML] (L.Html ())
  :<|> "search" :> QueryParam "q" [Keyword] :> Get '[HTML] (L.Html ())


instance FromHttpApiData [Keyword] where
  parseQueryParam = Right . fmap Keyword . T.split (== ' ')

testApi :: Proxy TestApi
testApi = Proxy

home :: L.Html ()
home =
  L.html_ $ do
    L.head_ $
      L.title_ "Yo"
    L.body_ $
      L.form_ [ L.action_ "/search", L.method_ "GET" ] $ do
        L.input_ [ L.type_ "text", L.name_ "q" ]
        L.input_ [ L.type_ "submit", L.value_ "Search!" ]


search :: Maybe [Keyword] -> Handler (L.Html ())
search Nothing = pure $ "Give me some keywords, punk!"
search (Just kws) = do
  docs <- liftIO $ do
    Right conn <- acquire connectionSettings
    swid <- evaluateTerm conn $ Terms kws
    Right docs <- flip run conn $ statement () $ select $ limit 10 $ orderBy (d_rank >$< desc) $ compileSearch swid
    pure docs
  pure $
    L.html_ $ do
      L.head_ $
        L.title_ $ "Search Results for " <> fromString (show kws)
      L.body_ $
        for_ docs $ searchResult

searchResult :: Discovery Rel8.Result -> L.Html ()
searchResult d =
  L.p_ $ do
    L.a_ [L.href_ $ d_uri d] $ fromString $ T.unpack $ d_title d
    L.br_ []
    L.a_ [L.href_ $ d_uri d] $ fromString $ T.unpack $ d_uri d

--   where
--     d_title :: Discovery Rel8.Result -> Text
--     d_title = fromMaybe "no title" . flip (runRanker undefined) title . decodeUtf8 . d_data


-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server TestApi
server = pure home :<|> search

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: W.Port -> IO ()
runTestServer port = W.run port test

-- Put this all to work!
main :: IO ()
main = runTestServer 8001

