module Data.SchemaOrg where

import Data.Aeson
import Data.Foldable (asum)


newtype IsAccessibleForFree = IsAccessibleForFree
  { isAccessibleForFree :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON IsAccessibleForFree where
  parseJSON = withObject "" $ \obj ->
    asum
      [ IsAccessibleForFree <$>  obj .: "isAccessibleForFree"
      , obj .: "hasPart"
      , pure $ IsAccessibleForFree True
      ]

