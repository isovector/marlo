{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8.Arrays where

import Rel8
import Data.Int (Int16, Int32)
import Opaleye.Internal.HaskellDB.PrimQuery
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (null)


insertAt' :: forall a. (DBType a, Sql DBType a) => Expr Int16 -> Expr Int16 -> Expr (Maybe a) -> Expr [Maybe a]
insertAt' sz ix a
  = binaryOperator "||" (arrayFill @(Maybe a) ix null)
  $ arrayPrepend a
  $ arrayFill (sz - ix - 1) null

arrayPrepend :: Sql DBType a => Expr a -> Expr [a] -> Expr [a]
arrayPrepend = function "array_prepend"

arrayInc :: (Sql DBType a, Sql Num a) => Expr [a] -> Expr [a]
arrayInc = function "array_inc"

arrayZipWithLeast :: Sql DBOrd a => Expr [a] -> Expr [a] -> Expr [a]
arrayZipWithLeast = function "array_zip_with_least"

arrayZipWithLt :: Sql DBOrd a => Expr [Maybe a] -> Expr [Maybe a] -> Expr [Maybe Bool]
arrayZipWithLt = function "array_zip_with_lt"

-- | Ignores nothings :|
arrayAllTrue :: Expr [Maybe Bool] -> Expr Bool
arrayAllTrue = function "array_all_true"

sequenceExpr :: Sql DBType a => [Expr a] -> Expr [a]
sequenceExpr = unsafeToExpr . ArrayExpr . fmap fromExpr

arrayFill :: Sql DBType a => Expr Int16 -> Expr a -> Expr [a]
arrayFill sz a
  = function "array_fill" a
  $ sequenceExpr [sz]

arrayCardinality :: Expr [a] -> Expr Int32
arrayCardinality = function "cardinality"

arrayPositions :: Expr [a] -> Expr a -> Expr [Int32]
arrayPositions = function "array_positions"

(!!.) :: Sql DBType a => Expr [a] -> Expr Int16 -> Expr (Maybe a)
(!!.) arr ix = unsafeToExpr $ ArrayIndex (fromExpr $ arr) $ fromExpr ix

newtype Expr' a = Expr' PrimExpr

-- zipmin :: Expr [Maybe Int16] -> Expr [Maybe Int16] -> Expr [Maybe Int16]
-- zipmin a b = aggregate $ _


unsafeToExpr :: PrimExpr -> Expr a
unsafeToExpr = unsafeCoerce . Expr'

fromExpr :: Expr a -> PrimExpr
fromExpr (unsafeCoerce -> Expr' a) = a

