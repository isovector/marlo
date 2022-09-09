module Network.HttpUtils where

import           Control.Applicative (empty, (<|>))
import           Control.Concurrent.Async (runConcurrently, Concurrently (..))
import           Control.Monad (void)
import           Data.Monoid (First (..))
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8')
import           Marlo.Manager (marloManager)
import           Network.HTTP.Client
import           Network.HTTP.Types (statusCode, hLocation)
import           Network.URI
import           Utils


mkHEADRequest :: String ->  IO Request
mkHEADRequest = parseRequest . mappend "HEAD "

doHEADRequest :: String -> IO (Response ())
doHEADRequest turi = do
  req <- mkHEADRequest turi
  flip httpNoBody marloManager req


determineHttpsAvailability :: URI -> IO (Maybe URI)
determineHttpsAvailability uri = do
  let https = uri { uriScheme = "https:" }
  let http  = uri { uriScheme = "http:" }

  let check :: URI -> IO (Maybe URI)
      check u = do
        rq <- mkHEADRequest $ show u
        rh <- fmap void $ withResponseHistory rq marloManager pure
        let hd = hrFinalResponse rh
        case statusCode $ responseStatus hd of
          200 -> fmap Just $ locationHeader hd <|> pure (getUri $ hrFinalRequest rh)
          300 -> fmap Just $ locationHeader hd
          301 -> fmap Just $ locationHeader hd
          302 -> fmap Just $ locationHeader hd
          307 -> pure Nothing  -- too hard to deal with temporary redirects
          308 -> fmap Just $ locationHeader hd
          403 -> pure Nothing
          404 -> pure Nothing
          -- one day support 500s to retry later
          _   -> pure Nothing

      locationHeader :: Response () -> IO URI
      locationHeader hd =
        case lookup hLocation $ responseHeaders hd of
          Nothing -> empty
          Just bs ->
            maybe empty pure
              $ parseURI =<< (hush $ fmap T.unpack $ decodeUtf8' bs)


  fmap getFirst
    . runConcurrently
    . fmap (foldMap First)
    $ traverse Concurrently
        [ check https
        , check http
        , pure Nothing
        ]

