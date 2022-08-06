module Metric where

import Rel8
import DB
import Rel8.Arrays
import Prelude hiding (null)
import Data.Int (Int16, Int64, Int32)
import Hasql.Connection (acquire)
import Hasql.Session (run, statement)
import Utils (random)
import Data.Text (Text)
import Data.Function (fix)
import Types


wrongDistance :: Query (Expr DocId, Expr [Maybe Int16])
wrongDistance = do
  e <- each edgesSchema
  src <- each documentSchema
  where_ $ e_src e ==. d_docId src
  dst <- each documentSchema
  where_ $ e_dst e ==. d_docId dst
  where_ $ distCard (d_distance src) >. distCard (d_distance dst)
  pure (e_dst e, d_distance src)


propagateDistances :: Update Int64
propagateDistances = Update
  { target = documentSchema
  , from = wrongDistance
  , set = \(_ ,dist) row -> row
              { d_distance = arrayZipWithLeast (arrayInc dist) (d_distance row)
              }
  , updateWhere = \(d, _) row -> d_docId row ==. d
  , returning = NumberOfRowsAffected
  }


distCard :: Expr [Maybe Int16] -> Expr Int32
distCard dist
  = lit (fromIntegral $ numRootSites) - (arrayCardinality $ arrayPositions dist null)


findJoins :: Query ((Expr Text, Expr [Maybe Int16]), (Expr Text, Expr [Maybe Int16]))
findJoins = limit 1 $ orderBy random $ do
  e <- each edgesSchema
  src <- each documentSchema
  where_ $ e_src e ==. d_docId src
  where_ $ d_distance src /=. nullDist
  dst <- each documentSchema
  where_ $ e_dst e ==. d_docId dst
  where_ $ d_distance dst /=. nullDist
  where_ $ distCard (d_distance src) <. distCard (d_distance dst)
  pure ((d_uri src, d_distance src), (d_uri dst, d_distance dst))







metricMain :: IO ()
metricMain = do
  Right conn <- acquire connectionSettings
  (print =<<) $ flip run conn $ statement () $ insert rootNodes
  fix $ \loop -> do
    res <- flip run conn $ statement () $ update propagateDistances
    case res of
      Right 0 -> putStrLn "converged!"
      Right n -> do
        putStrLn $ "updated " <> show n <> " rows"
        loop
      Left err -> print err

