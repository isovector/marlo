module Network.HttpUtils where

import           Control.Applicative ((<|>), optional)
import           Control.Concurrent.Async (runConcurrently, Concurrently (..))
import           Control.Monad (void, join)
import           Data.Monoid (First (..))
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8')
import           GHC.Stack (HasCallStack)
import           Marlo.Manager (marloManager)
import           Network.HTTP.Client
import           Network.HTTP.Types (statusCode, hLocation)
import           Network.URI
import           Utils


mkHEADRequest :: HasCallStack => String ->  IO Request
mkHEADRequest = parseRequest . mappend "HEAD "

doHEADRequest :: HasCallStack => String -> IO (Response ())
doHEADRequest turi = do
  req <- mkHEADRequest turi
  flip httpNoBody marloManager req


determineHttpsAvailability :: HasCallStack => URI -> IO (Maybe URI)
determineHttpsAvailability uri = fmap join $ optional $ do
  let https = uri { uriScheme = "https:" }
  let http  = uri { uriScheme = "http:" }

  let check :: HasCallStack => URI -> IO (Maybe URI)
      check u = fmap join $ optional $ do
        rq <- mkHEADRequest $ show u
        rh <- fmap void $ withResponseHistory rq marloManager pure
        let hd = hrFinalResponse rh
        pure $ case statusCode $ responseStatus hd of
          200 -> locationHeader hd <|> (Just $ getUri $ hrFinalRequest rh)
          300 -> locationHeader hd
          301 -> locationHeader hd
          302 -> locationHeader hd
          307 -> Nothing  -- too hard to deal with temporary redirects
          308 -> locationHeader hd
          403 -> Nothing
          404 -> Nothing
          -- one day support 500s to retry later
          _   -> Nothing

      locationHeader :: Response () -> Maybe URI
      locationHeader hd =
        case lookup hLocation $ responseHeaders hd of
          Nothing -> Nothing
          Just bs ->
            parseURI =<< (hush $ fmap T.unpack $ decodeUtf8' bs)


  fmap getFirst
    . runConcurrently
    . fmap (foldMap First)
    $ traverse Concurrently
        [ check https
        , check http
        , pure Nothing
        ]

