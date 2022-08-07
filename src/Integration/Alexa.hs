module Integration.Alexa
  ( getGlobalRank
  ) where

import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Client hiding (Proxy)
import Servant
import Servant.Client (client, runClientM, BaseUrl(..), Scheme (Http), mkClientEnv, ClientM)
import System.IO.Unsafe (unsafePerformIO)


getGlobalRank :: Text -> IO (Maybe Int)
getGlobalRank uri = do
  let alexa = BaseUrl Http "data.similarweb.com" 80 ""
  let env = mkClientEnv alexaManager alexa
  fmap (either (pure Nothing) (Just . ar_globalRank))
    $ runClientM (getAlexaResult uri) env



newtype AlexaResult = AlexaResult
  { ar_globalRank :: Int
  }
  deriving newtype (Eq, Ord, Show)

instance FromJSON AlexaResult where
  parseJSON =
    withObject "Root" $ \obj -> do
      gro <- obj .: "GlobalRank"
      flip (withObject "GlobalRank") gro $ \gr ->
        AlexaResult <$> gr .: "Rank"

-- https://data.similarweb.com/api/v1/data?domain=https://neocities.org
type AlexaAPI = "api" :> "v1" :> "data" :> QueryParam "domain" Text :> Get '[JSON] AlexaResult


getAlexaResult :: Text -> ClientM AlexaResult
getAlexaResult = client (Proxy @AlexaAPI) . Just


alexaManager :: Manager
alexaManager = unsafePerformIO $
  newManager $ defaultManagerSettings
    { managerModifyRequest = \req ->
        pure $ req
          { requestHeaders =
              [ ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.114 Safari/537.36I")
              , ("accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9")
              , ("accept-encoding", "gzip, deflate, br")
              , ("accept-language", "en-GB;q=0.6")
              , ("cache-control", "max-age=0")
              , ("sec-fetch-dest", "document")
              , ("sec-fetch-mode", "navigate")
              , ("sec-fetch-site", "none")
              , ("sec-fetch-user", "?1")
              , ("sec-gpc", "1")
              , ("upgrade-insecure-requests", "1")
              ] <> requestHeaders req
          }
    }
