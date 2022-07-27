{-# LANGUAGE LambdaCase #-}

module Rel8.TextSearch where

import           Data.Text (Text)
import qualified Data.Text as T
import           Rel8 hiding (index)
import           Servant.Server.Generic ()
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Prim
import qualified Hasql.Decoders as Decode
import Data.Int (Int16)

data Tsvector = Tsvector
  deriving (Eq, Ord, Show)

instance DBEq Tsvector

instance DBOrd Tsvector

instance DBType Tsvector where
  typeInformation = TypeInformation
    { encode = const $ Prim.ConstExpr $ Prim.StringLit ""
    , decode = Decode.custom $ \_ _ -> pure Tsvector
    , typeName = "tsvector"
    }

data Tsquery
  = TsqTerm Text
  | TsqAnd Tsquery Tsquery
  | TsqOr Tsquery Tsquery
  | TsqNot Tsquery
  | TsqPhrase Tsquery Tsquery
  deriving (Eq, Ord, Show)


instance DBType Tsquery where
  typeInformation = TypeInformation
    { encode = \case
        TsqTerm txt ->
          Prim.CastExpr "tsquery" $ Prim.ConstExpr $ Prim.StringLit $ T.unpack txt
        TsqAnd lhs rhs ->
          Prim.BinExpr (Prim.OpOther "&&") (encode typeInformation lhs) (encode typeInformation rhs)
        TsqOr lhs rhs ->
          Prim.BinExpr (Prim.OpOther "||") (encode typeInformation lhs) (encode typeInformation rhs)
        TsqNot lhs ->
          Prim.UnExpr (Prim.UnOpOther "!!") (encode typeInformation lhs)
        TsqPhrase lhs rhs ->
          Prim.BinExpr (Prim.OpOther "<->") (encode typeInformation lhs) (encode typeInformation rhs)
    , decode = Decode.custom $ const $ \bs -> error "need to parse still"
    , typeName = "tsquery"
    }

headline :: Expr Text -> Expr Tsquery -> Expr Text
headline = function "ts_headline"

match :: Expr Tsvector -> Expr Tsquery -> Expr Bool
match = binaryOperator "@@"

rank :: Expr Tsvector -> Expr Tsquery -> Expr Int16 -> Expr Float
rank = function "ts_rank_cd"


rIGNORE :: Expr Int16
rIGNORE = lit 0

rLOGLENGTH :: Expr Int16
rLOGLENGTH = lit 1

rLENGTH :: Expr Int16
rLENGTH = lit 2

rDISTANCE :: Expr Int16
rDISTANCE = lit 4

rUNIQUES :: Expr Int16
rUNIQUES = lit 8

rLOGUNIQUES :: Expr Int16
rLOGUNIQUES = lit 16

rRANK :: Expr Int16
rRANK = lit 32

