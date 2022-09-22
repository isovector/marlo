module Data.SchemaOrg where

import Data.Aeson
import Data.Foldable (asum)
import Data.Text (Text)


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



newtype MetadataType = MetadataType
  { getMetadataType :: Text
  }
  deriving (Eq, Ord, Show)


-- TODO(sandy): some documents have an array of many of these things!
instance FromJSON MetadataType where
  parseJSON = withObject "" $ \obj ->
    asum
      [ MetadataType <$>  obj .: "@type"
      ]

