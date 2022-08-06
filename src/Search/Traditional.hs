module Search.Traditional
  ( traditionalSearch
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           DB
import           Data.Foldable (for_)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Hasql.Connection (Connection)
import           Hasql.Session (run, statement)
import qualified Lucid as L
import           Network.URI (escapeURIString, isUnescapedInURI)
import           Rel8 hiding (max, index)
import           Rel8.TextSearch
import           Search.Compiler
import           Search.SearchBar (searchBar)
import           Servant
import           Servant.Server.Generic ()
import           Types
import           Utils (paginate, timing)
import Data.List (intersperse)



getSnippet :: DocId -> Tsquery -> Query (Expr Text)
getSnippet did q = do
  d <- d_table <$> each discoverySchema'
  where_ $ d_docId d ==. lit did
  let pc = d_page d
  pure
    $ headline
        (foldr1 (<>.)
          $ intersperse " "
            [ pc_content pc
            , pc_headings pc
            , pc_comments pc
            ])
    $ lit q


traditionalSearch :: Connection -> Search Text -> Maybe Int -> Handler (L.Html ())
traditionalSearch conn q mpage = do
  let pagenum = Prelude.max 0 $ maybe 0 (subtract 1) mpage
      pagesize :: Num a => a
      pagesize = 20
  (cnt, docs, snips) <- liftIO $ do
    putStrLn $ mappend "trad search: " $ T.unpack $ encodeQuery q
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
          $ getSnippet (sr_id doc) $ compileQuery q
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
          searchBar Traditional $ encodeQuery q
          for_ (zip docs snips) $ uncurry tradResult
          let eq = escape $ encodeQuery q
          when (pagenum > 0) $ do
            L.a_ [L.href_ $ "/search?q=" <> eq <> "&p=" <> T.pack (show pagenum)  ] "Prev"
          when ((pagenum + 1) * pagesize < fromIntegral cnt) $ do
            L.a_ [L.href_ $ "/search?q=" <> eq <> "&p=" <> T.pack (show (pagenum + 2))  ] "Next"
  where
    escape = T.pack . escapeURIString isUnescapedInURI . T.unpack


tradResult :: SearchResult Rel8.Result -> Text -> L.Html ()
tradResult d snip =
  L.div_ [L.class_ "result"] $ do
    L.span_ [L.class_ "url"] $ L.a_ [L.href_ $ sr_uri d] $ L.toHtml $ sr_uri d
    L.span_ [L.class_ "title"] $ L.a_ [L.href_ $ sr_uri d] $ L.toHtml title
    L.p_ [L.class_ "snippet"] $ L.toHtmlRaw snip
  where
    title =
      case T.strip $ sr_title d of
        "" -> "(no title)"
        x -> x

