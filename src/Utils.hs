{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Utils
  ( module Utils
  , hush
  ) where


import           Control.Applicative ((<|>))
import           Control.Arrow (first)
import           Control.Monad.Reader
import           DB
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Foldable (for_)
import           Data.Functor.Contravariant ((>$))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime, NominalDiffTime)
import           Data.Time.Clock (diffUTCTime)
import           Marlo.Manager (marloManager)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types (hContentType)
import           Network.URI
import           Rel8 hiding (filter)
import           Rel8.Machinery
import           Text.HTML.Scalpel
import           Text.Printf (printf)
import           Types


paginate size page q =
  limit size $
    offset (page * size) q


runRanker :: Env -> Text -> Ranker a -> IO (Maybe a)
runRanker e t = flip runReaderT e . scrapeStringLikeT t


countOf :: Selector -> Ranker Int
countOf sel = fmap length $ chroots sel $ pure ()


tagClass :: String -> String -> Selector
tagClass a b = TagString a @: [hasClass b]

tagId :: String -> String -> Selector
tagId a b = TagString a @: ["id" @= b]


classOf :: String -> Selector
classOf c = AnyTag @: [hasClass c]


unsafeURI :: String -> URI
unsafeURI = fromJust . parseURI


currentURI :: Ranker URI
currentURI = asks e_uri


withClass :: Selector -> (Text -> Bool) -> Ranker ()
withClass s f = do
  cls <- T.words <$> attr "class" s
  guard $ any f cls


has :: Ranker a -> Ranker Bool
has r = True <$ r <|> pure False


posKeywordsToInv :: [(Int, Text)] -> Set Text
posKeywordsToInv = S.fromList . fmap snd


timeItT :: IO a -> IO (NominalDiffTime, a)
timeItT ioa = do
    t1 <- getCurrentTime
    a <- ioa
    t2 <- getCurrentTime
    return (diffUTCTime t2 t1, a)


timing :: String -> IO a -> IO a
timing name ioa = do
    (t, a) <- fmap (first $ realToFrac @_ @Double) $ timeItT ioa
    liftIO $ printf (name ++ ": %6.4fs\n") t
    pure a


random :: Order a
random = nullaryFunction @Double "RANDOM" >$ asc


insertAt :: Int -> Int -> a -> [Maybe a]
insertAt (subtract 1 -> sz) ix a = replicate ix Nothing <> (Just a : replicate (sz - ix) Nothing)



commafy :: String -> String
commafy
  = T.unpack
  . T.intercalate ","
  . reverse
  . fmap T.reverse
  . T.chunksOf 3
  . T.reverse
  . T.pack


downloadBody :: String -> IO (Download Maybe ByteString)
downloadBody url = do
  resp <- flip HTTP.httpLbs marloManager =<< HTTP.parseRequest url
  let mime = fmap mimeToContentType
            $ lookup hContentType
            $ HTTP.responseHeaders resp
  pure
    $ Download mime (HTTP.responseHeaders resp)
    $ BSL.toStrict $ HTTP.responseBody resp


mimeToContentType :: ByteString -> ByteString
mimeToContentType = BS.takeWhile (/= ';')


withDocuments
    :: Connection
    -> (Document Expr -> Expr Bool)
    -> (Document Result -> IO b)
    -> IO ()
withDocuments conn sel m = do
  Right docs <- doSelect conn $ do
    d <- each documentSchema
    where_ $ sel d
    pure $ d_docId d
  for_ docs $ \did -> do
    Right [doc] <-
      doSelect conn $ do
        d <- each documentSchema
        where_ $ d_docId d ==. lit did
        pure d
    m doc


titleSegs :: Text -> [Text]
titleSegs
  = filter (not . T.null)
  . fmap T.strip
  . concatMap (T.splitOn ". ")
  . concatMap (T.splitOn " - ")
  . T.split (flip elem [';', ':', '|', '·', '«', '»', '∷', '>', '<', '\8211' ])


