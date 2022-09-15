module Rel8.StateMask where

import           Control.Arrow ((&&&))
import           Control.DeepSeq (NFData)
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC.Exts (IsList(..))
import           Hasql.Decoders (int8)
import           Opaleye.Internal.HaskellDB.PrimQuery
import           Rel8 hiding (Enum)


toBitMask :: Enum a => a -> Integer
toBitMask a = 2 ^ fromEnum a


newtype BitMask a = BitMask
  { getBitMask :: Set a
  }
  deriving stock (Eq, Ord)
  deriving newtype (Semigroup, Monoid, NFData, Show)

instance (Ord a, Bounded a, Enum a) => DBSemigroup (BitMask a) where
  (<>.) = binaryOperator "|"


instance Ord a => IsList (BitMask a) where
  type Item (BitMask a) = a
  fromList = BitMask . S.fromList
  toList = S.toList . getBitMask

flag :: a -> BitMask a
flag = BitMask . S.singleton

flagIf :: a -> Bool -> BitMask a
flagIf _ False = BitMask S.empty
flagIf a True = flag a


isEmpty :: BitMask a -> Bool
isEmpty = S.null . getBitMask


instance (Ord a, Bounded a, Enum a) => DBType (BitMask a) where
  typeInformation = TypeInformation
    { encode = \(BitMask s)
         -> ConstExpr
          $ IntegerLit
          $ Prelude.sum
          $ fmap toBitMask
          $ S.toList s
    , decode = fmap (fromBitMask . fromIntegral) int8
    , typeName = "int8"
    }

instance (Ord a, Bounded a, Enum a) => DBEq (BitMask a)


fromBitMask :: forall a. (Ord a, Enum a, Bounded a) => Integer -> BitMask a
fromBitMask
  = BitMask
  . S.fromList
  . go ( fmap (id &&& toBitMask)
       $ reverse [minBound @a .. maxBound]
       )
  where
    go :: [(a, Integer)] -> Integer -> [a]
    go _ 0 = []
    go [] _ = error "impossible: bug in fromBitMask"
    go ((a, i) : xs) n
      | n >= i = a : go xs (n - i)
      | otherwise = go xs n

