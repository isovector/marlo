{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module WebGraph where

import DB
import Rel8
import Control.Applicative
import Data.Int (Int64)
import Hasql.Statement
import Hasql.Connection (Connection, acquire)
import Hasql.Session
import Data.Graph.PageRank (pageRanks)
import Data.Map (toDescList)
import Streaming (Stream, Of)
import qualified Streaming as S
import qualified Streaming.ByteString.Char8 as SB
import qualified Streaming.Prelude as S
import Data.ByteString.Streaming.HTTP (MonadResource, runResourceT)
import Data.ByteString.Char8 (ByteString)
import Data.Function ((&))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Bifunctor (bimap)
import Control.Monad.Trans (lift)

pagerankCsv :: forall m. MonadResource m => Stream (Of (DocId, Double)) m ()
pagerankCsv
  = SB.readFile @m "/home/sandy/pagerank.csv"
  & SB.lines
  & S.mapped SB.toStrict
  & S.drop 1
  & S.map (break (== ',') . T.unpack . decodeUtf8)
  & S.map (bimap (DocId . read) $ read . tail)

main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  runResourceT $ do
    flip S.mapM_ (S.zip (S.each [0..]) pagerankCsv) $ \x@(n, (d, pr)) -> lift $ do
      print n
      flip run conn $ statement () $ update $ Update
        { target = discoverySchema
        , from = pure ()
        , set = const $ \e -> e { d_rank = lit pr }
        , updateWhere = const $ \e -> d_docId e ==. lit d
        , returning = pure ()
        }

  -- gr <- buildGraph conn
  -- print "built graph"
  -- let pr = pageRanks gr 0.1 0.1
  -- print $ head $ toDescList $ pr

-- buildGraph :: Connection -> IO (Gr () ())
-- buildGraph conn = do
--   Right nodes <- flip run conn $ statement () $ select selNodes
--   print "got nodes"
--   Right edges <- flip run conn $ statement () $ select selEdges
--   print "got edges"
--   let f = fromIntegral . unDocId
--   pure $ mkGraph (fmap (\ di -> (f di, ())) nodes) (fmap (\ (src, dst) -> (f src, f dst, ())) edges)


-- selEdges :: Query (Expr DocId, Expr DocId)
-- selEdges = do
--   Edges _ src dst _ <- each edgesSchema
--   pure $ (src, dst)


-- selNodes :: Query (Expr DocId)
-- selNodes = do
--   d <- each discoverySchema
--   pure $ d_docId d


