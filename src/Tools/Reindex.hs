module Tools.Reindex where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe
import           DB
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (decodeUtf8)
import           Marlo.Filestore (streamFilestore)
import           Network.HttpUtils (determineHttpsAvailability)
import           Signals (canonical)
import           Spider (reindex, getDocByCanonicalUri)
import qualified Streaming.Prelude as S
import           Types
import           Utils (runRanker)


main :: IO ()
main = do
  Right conn <- connect
  S.effects
    $ S.mapM (uncurry $ reindex conn)
    $ S.mapM (\x -> print (fs_uri $ snd x) >> pure x)
    $ S.mapMaybeM (canonicalizing conn)
    $ streamFilestore


canonicalizing :: Connection -> Filestore -> IO (Maybe (DocId, Filestore))
canonicalizing conn fs = do
  let uri = fs_uri fs
  uri'  <- runRanker (Env uri conn) (decodeUtf8 $ fs_data fs) canonical
  runMaybeT $ do
    uri'' <- MaybeT $ determineHttpsAvailability $ fromMaybe uri uri'
    doc <- liftIO $ getDocByCanonicalUri conn uri''
    let did = either id d_docId doc
    pure (did, fs { fs_uri = uri })

