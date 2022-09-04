module Tools.BatchTitles where

import           Control.Monad (when, void)
import           DB
import           Data.Foldable (for_)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import           Rel8
import           Marlo.TitleSegs (setBestTitle, buildTitleSegs)
import           Types


main :: IO ()
main = do
  Right conn <- connect
  Right docs <- doSelect conn $ do
    d <- each documentSchema
    pure $ d_docId d
  for_ docs $ \did -> do
    Right [doc] <-
      doSelect conn $ do
        d <- each documentSchema
        where_ $ d_docId d ==. lit did
        pure d
    let docid = d_docId doc
    Right num <- doSelect conn $ numTitleSegs docid
    when (fromMaybe 0 (listToMaybe num) == 0) $
      void $ buildTitleSegs conn docid $ d_title doc

    case d_title doc of
      "" -> pure ()
      _ -> do
        mtit <- doUpdate conn $ setBestTitle $ d_docId doc
        for_ mtit $ \tits -> do
          for_ tits $ \tit -> do
            putStrLn $ T.unpack $ mconcat
              [ d_uri doc
              , " -> "
              , tit
              ]


numTitleSegs :: DocId -> Query (Expr (Int64))
numTitleSegs docid = fmap snd $ aggregate $ do
  te <- each titleEdgeSchema
  where_ $ te_doc te ==. lit docid
  pure (groupBy $ te_doc te, countStar)


