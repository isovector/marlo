{-# LANGUAGE FunctionalDependencies #-}

module Rel8.Machinery where

import Rel8


class Rel8able f => HasUniqueId f uniq | f -> uniq, uniq -> f where
  allRows :: TableSchema (f Name)
  uniqueId :: f Expr -> Expr uniq
  nextUniqueId :: Query (Expr uniq)

