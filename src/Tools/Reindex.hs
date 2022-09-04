module Tools.Reindex where

import           DB
import           Marlo.Filestore (streamFilestore)
import           Spider (reindex, getDocByCanonicalUri, getCanonicalUri)
import qualified Streaming.Prelude as S
import           Types


main :: IO ()
main = do
  Right conn <- connect
  S.effects
    $ S.mapM (uncurry $ reindex conn)
    $ S.mapM (\x -> print (fs_uri $ snd x) >> pure x)
    $ S.mapM (canonicalizing conn)
    $ streamFilestore


canonicalizing :: Connection -> Filestore -> IO (DocId, Filestore)
canonicalizing conn fs = do
  Just (_, uri) <- getCanonicalUri conn $ fs_uri fs
  doc <- getDocByCanonicalUri conn uri
  let did = either id d_docId doc
  pure (did, fs { fs_uri = uri })

