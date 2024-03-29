module Tools.Reindex where

import           Control.Exception (evaluate)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe
import           DB
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Marlo.Filestore (streamFilestore)
import           Network.HttpUtils (determineHttpsAvailability)
import           Rel8 (in_, lit)
import           Signals.Content (canonical)
import           Spider (reindex, getDocByCanonicalUri, markDiscovered)
import qualified Streaming.Prelude as S
import           Types
import           Utils (parsePermissiveTree, runScraper)


main :: Maybe Int -> IO ()
main start = do
  Right conn <- connect
  S.effects
    $ S.mapM (uncurry $ reindex conn)
    $ S.mapM (\x -> print (fs_uri $ snd x) >> pure x)
    $ S.mapMaybeM (canonicalizing conn)
    $ S.mapM (\(ix, a) -> writeFile "/tmp/marlo-reindex" (show @Integer ix) >> pure a)
    $ S.drop (maybe 0 (+1) start)
    $ S.zip (S.each [0..])
    $ streamFilestore


canonicalizing :: Connection -> Filestore -> IO (Maybe (DocId, Filestore))
canonicalizing conn fs = do
  let uri = fs_uri fs
  let !tree = parsePermissiveTree $ decodeUtf8 $ fs_data fs
      runL
        = runScraper
        $ fromMaybe (error "failed to parse the html document!") tree
  !uri'  <- evaluate $ runL canonical
  runMaybeT $ do
    uri'' <- MaybeT $ determineHttpsAvailability $ fromMaybe uri uri'
    doc <- liftIO $ getDocByCanonicalUri conn uri''
    let did = either id d_docId doc

    liftIO
      $ markDiscovered conn (Just did)
      $ \d -> in_ (disc_uri d)
            $ fmap lit
            $ fmap (T.pack . show)
            $ maybeToList uri' <>  [uri, uri'']

    pure (did, fs { fs_uri = uri })

