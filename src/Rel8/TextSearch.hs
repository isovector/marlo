{-# OPTIONS_GHC -Wno-orphans #-}

module Rel8.TextSearch where

import           Data.Int (Int16)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Hasql.Decoders as Decode
import           Opaleye.Internal.HaskellDB.PrimQuery (PrimExpr)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Prim
import           Rel8 hiding (Enum, index)
import           Servant.Server.Generic ()

data Label = A | B | C | D
  deriving (Eq, Ord, Show, Enum, Bounded)

data Tsvector = Tsvector [(Label, Text)]
  deriving (Eq, Ord, Show)

instance DBEq Tsvector

instance DBOrd Tsvector

instance DBType Tsvector where
  typeInformation = TypeInformation
    { encode = \(Tsvector ts) ->
        foldr
          (\a b -> uncurry mkWeightedTsVec a ||.. " " ||.. b)
          ""
          ts
    , decode = error "can't decode tsvector yet"
    , typeName = "tsvector"
    }

(||..) :: PrimExpr -> PrimExpr -> PrimExpr
a ||.. b = Prim.BinExpr (Prim.:||) a b

infixr 5 ||..


mkWeightedTsVec :: Label -> Text -> Prim.PrimExpr
mkWeightedTsVec l t = Prim.FunExpr "setweight"
  [ Prim.FunExpr "to_tsvector"
      [ "english"
      , fromString $ T.unpack t
      ]
  , fromString $ show l
  ]

instance IsString Prim.PrimExpr where
  fromString = Prim.ConstExpr . Prim.StringLit

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
          Prim.FunExpr "to_tsquery"
            [ Prim.ConstExpr $ Prim.StringLit $ T.unpack txt ]
        TsqAnd lhs rhs ->
          Prim.BinExpr (Prim.OpOther "&&") (encode typeInformation lhs) (encode typeInformation rhs)
        TsqOr lhs rhs ->
          Prim.BinExpr (Prim.OpOther "||") (encode typeInformation lhs) (encode typeInformation rhs)
        TsqNot lhs ->
          Prim.UnExpr (Prim.UnOpOther "!!") (encode typeInformation lhs)
        TsqPhrase lhs rhs ->
          Prim.BinExpr (Prim.OpOther "<->") (encode typeInformation lhs) (encode typeInformation rhs)
    , decode = Decode.custom $ const $ \_ -> error "need to parse still"
    , typeName = "tsquery"
    }

headline :: Expr Text -> Expr Tsquery -> Expr Text
headline a b
  = function "ts_headline" a b
  $ lit
  $ id @Text "ShortWord=2, MaxFragments=8, MaxWords=12, MinWords=5"

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

