{-# OPTIONS_GHC -Wno-orphans #-}

module Rel8.Headers
  ( Header
  , module Rel8.Headers
  ) where

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Hasql.Decoders as Decode
import           Network.HTTP.Headers
import qualified Network.HTTP.Types.Header
import           Opaleye.Internal.HaskellDB.PrimQuery
import           Rel8


instance DBEq Header

instance DBType Header where
  typeInformation = TypeInformation
    { encode = \(Header hn v) -> ConstExpr $ StringLit $ show hn <> ": " <> v
    , decode = Decode.custom $ const $ \bs ->
        case parseHeader $ T.unpack $ decodeUtf8 bs of
          Left ce -> Left $ T.pack $ show ce
          Right he -> Right he
    , typeName = "text"
    }

headersToHeaders :: Network.HTTP.Types.Header.Header -> Network.HTTP.Headers.Header
headersToHeaders (ci, bs) = Header (getHeaderName $ show ci) $ T.unpack $ decodeUtf8 bs

getHeaderName :: String -> HeaderName
getHeaderName s = fromMaybe (HdrCustom s) $ lookup s headerMap

