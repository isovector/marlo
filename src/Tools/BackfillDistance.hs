module Tools.BackfillDistance where

import DB
import Data.Function (fix)
import Data.Int (Int16, Int64, Int32)
import Data.Text (Text)
import Prelude hiding (null)
import Rel8
import Rel8.Arrays
import Types
import Utils (random)
import Data.Coerce (coerce)


wrongDistance :: Query (Expr DocId, Expr (Distance Int16))
wrongDistance = do
  e <- each edgesSchema
  src <- each documentSchema
  where_ $ e_src e ==. d_docId src
  dstdisc <- each discoverySchema
  where_ $ e_dst e ==. disc_id dstdisc
  dst <- each documentSchema
  where_ $ disc_canonical dstdisc ==. nullify (d_docId dst)
  where_ $ distCard (d_distance src) >. distCard (d_distance dst)
  pure (d_docId dst, d_distance src)


propagateDistances :: Update Int64
propagateDistances = Update
  { target = documentSchema
  , from = wrongDistance
  , set = \(_ ,dist) row -> row
              { d_distance = coerce (arrayZipWithLeast @(Maybe Int16)) (incDistance dist) (d_distance row)
              }
  , updateWhere = \(d, _) row -> d_docId row ==. d
  , returning = NumberOfRowsAffected
  }


distCard :: Expr (Distance Int16) -> Expr Int32
distCard (viewAs -> dist)
  = lit (fromIntegral $ numRootSites) - (arrayCardinality $ arrayPositions dist null)


findJoins :: Query ((Expr Text, Expr (Distance Int16)), (Expr Text, Expr (Distance Int16)))
findJoins = limit 1 $ orderBy random $ do
  e <- each edgesSchema
  src <- each documentSchema
  where_ $ e_src e ==. d_docId src
  where_ $ d_distance src /=. nullDist
  dstdisc <- each discoverySchema
  where_ $ e_dst e ==. disc_id dstdisc
  dst <- each documentSchema
  where_ $ disc_canonical dstdisc ==. nullify (d_docId dst)
  where_ $ d_distance dst /=. nullDist
  where_ $ distCard (d_distance src) <. distCard (d_distance dst)
  pure ((d_uri src, d_distance src), (d_uri dst, d_distance dst))


main :: IO ()
main = do
  Right conn <- connect
  (print =<<) $ doInsert conn rootNodes
  fix $ \loop -> do
    res <- doUpdate conn propagateDistances
    case res of
      Right 0 -> putStrLn "converged!"
      Right n -> do
        putStrLn $ "updated " <> show n <> " rows"
        loop
      Left err -> print err

