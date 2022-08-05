module Rel8.Headers
  ( Header
  , module Rel8.Headers
  ) where

import           Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Hasql.Decoders as Decode
import           Network.HTTP.Headers
import           Opaleye.Internal.HaskellDB.PrimQuery
import           Rel8
import qualified Network.HTTP.Types.Header
import Data.String (fromString)


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
headersToHeaders (ci, bs) = Header (HdrCustom $ show ci) $ T.unpack $ decodeUtf8 bs

