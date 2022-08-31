{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Rel8.Machinery where

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Hasql.Connection (Connection)
import           Hasql.Session (QueryError, statement, run)
import           Rel8


doInsert :: Connection -> Insert a -> IO (Either QueryError a)
doInsert conn = flip run conn . statement () . insert


doSelect
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Query exprs
    -> IO (Either QueryError [FromExprs exprs])
doSelect conn = flip run conn . statement () . select


doUpdate :: Connection -> Update a -> IO (Either QueryError a)
doUpdate conn = flip run conn . statement () . update


doDelete :: Connection -> Delete a -> IO (Either QueryError a)
doDelete conn = flip run conn . statement () . delete



class Rel8able f => HasUniqueId f uniq | f -> uniq, uniq -> f where
  allRows :: TableSchema (f Name)
  uniqueId :: f Expr -> Expr uniq
  nextUniqueId :: Query (Expr uniq)


getOrInsert
    :: (HasUniqueId f key, Ord a, _)
    => Connection
    -> (Expr key -> Expr a -> f Expr)
    -> (f Expr -> Expr a)
    -> [a]
    -> IO (Map a key)
getOrInsert conn build what as = do
  let all_vals = S.fromList as
  Right vals <- fmap (fmap M.fromList) $ doSelect conn $ do
    r <- each allRows
    where_ $ in_ (what r) $ fmap lit as
    pure (what r, uniqueId r)
  let missing_vals = all_vals S.\\ M.keysSet vals
  Right more_vals <- fmap (fmap M.fromList) $ doInsert conn $ Insert
    { into = allRows
    , rows = do
        rid <- nextUniqueId
        a <- values $ fmap lit $ S.toList missing_vals
        pure $ build rid a
    , onConflict = DoNothing
    , returning = Projection $ \r -> (what r, uniqueId r)
    }
  pure $ vals <> more_vals

