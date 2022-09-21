module Marlo.TitleSegs where

import DB
import Data.Foldable (toList)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Prelude hiding (max)
import Rel8 hiding (filter, bool, index)
import Types
import Utils (titleSegs)


buildTitleSegs :: Connection -> DocId -> Text -> IO Text
buildTitleSegs conn doc t = do
  let segs = titleSegs t
  segids <- getOrInsert conn TitleSeg ts_seg segs
  Right _ <- doInsert conn $ Insert
    { into = titleEdgeSchema
    , rows = do
        teid <- nextTitleEdgeId
        seg <- values $ fmap lit $ toList segids
        pure $ TitleEdge
          { te_id = teid
          , te_doc = lit doc
          , te_seg = seg
          }
    , onConflict = DoNothing
    , returning = pure ()
    }
  Right [res] <- doSelect conn $ getBestTitle doc
  pure res


commonTitleSegs :: Query (Expr TitleSegId, Expr Int64)
commonTitleSegs = do
  orderBy ((snd >$< asc) <> (fst >$< asc)) $ aggregate $ do
    te <- each titleEdgeSchema
    pure (groupBy $ te_seg te, countStar)


getBestTitle :: DocId -> Query (Expr Text)
getBestTitle did = do
  segid <- limit 1 $ fmap fst $ orderBy (snd >$< asc) $ do
    te <- each titleEdgeSchema
    where_ $ te_doc te ==. lit did
    res <- commonTitleSegs
    where_ $ te_seg te ==. fst res
    pure res
  seg <- each titleSegSchema
  where_ $ ts_id seg ==. segid
  pure $ ts_seg seg


setBestTitle :: DocId -> Update [Text]
setBestTitle did = Update
  { target = documentSchema
  , from = getBestTitle did
  , set = \ ex doc -> doc { d_title = ex }
  , updateWhere = \ _ doc -> d_docId doc ==. lit did
  , returning = Projection d_title
  }

