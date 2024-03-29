{-# OPTIONS_GHC -Wno-orphans #-}

module Search.Traditional (getSnippet') where

import           DB
import           Data.Foldable (traverse_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Lucid as L
import           Rel8 hiding (max, index)
import           Rel8.TextSearch
import           Search.Compiler
import           Search.Machinery
import           Servant.Server.Generic ()
import           Servant.StreamingUtil
import           Types



getSnippet :: DocId -> Tsquery -> Query (Expr Text)
getSnippet did q = do
  d <- each documentSchema'
  where_ $ d_docId (d_table d) ==. lit did
  getSnippet' d q

getSnippet' :: Document' Expr -> Tsquery -> Query (Expr Text)
getSnippet' d =
  pure
    . headline (d_doc_text d)
    . lit


instance SearchMethod 'Traditional where
  type SearchMethodResult 'Traditional = [(SearchResult Identity, Text)]

  limitStrategy = Paginate 20

  accumResults conn _ _ q docs = do
    let q' = compileQuery q
    for docs $ \doc -> do
      Right [snip] <- doSelect conn $ getSnippet (sr_id doc) q'
      pure (doc, snip)

  showResults _ _ = yield . traverse_ (uncurry tradResult)

  debugResults = traverse_ $ uncurry $ \r snip -> do
    putStrLn $ T.unpack $ sr_title r
    putStrLn $ T.unpack $ snip
    putStrLn "\n"


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

