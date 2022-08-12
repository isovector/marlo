module Marlo.Manager where

import Network.HTTP.Client
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Data.String (IsString)


marloUserAgent :: IsString a => a
marloUserAgent = "marlo"


marloManager :: Manager
marloManager = unsafePerformIO $ do
  newTlsManagerWith $ tlsManagerSettings
    { managerModifyRequest = \req ->
        pure $ req
          { requestHeaders =
              [ ("User-Agent", marloUserAgent <> "/1.0 (+https://marlo.sandymaguire.me)")
              ] <> requestHeaders req
          }
    }
{-# NOINLINE marloManager #-}

