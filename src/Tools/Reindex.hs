module Tools.Reindex where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))
import           DB
import           Data.Foldable (traverse_)
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import           GHC.Stack (HasCallStack)
import           Lasercutter
import           Lasercutter.HTML (scrape, match, contentText, html, (/\), targetOne, debugTree, debugForest, (\/), text, expectSome, flattenedText)
import           Marlo.Filestore (streamFilestore)
import           Marlo.ScrapeContent (mainContent, headingsContent)
import           Network.HttpUtils (determineHttpsAvailability)
import           Signals (canonical)
import           Spider (reindex, getDocByCanonicalUri)
import qualified Streaming.Prelude as S
import           Text.HTML.TagSoup.PermissiveTree (parsePermissiveTree)
import           Text.HTML.TagSoup.Tree (parseTree, TagTree (TagLeaf, TagBranch))
import           Types
import           Utils (runRanker)
import Marlo.ScrapeContent (rankContent)


main :: HasCallStack => IO ()
main = do
  Right conn <- connect
  S.effects
    $ S.mapM (uncurry $ reindex' conn)
    -- $ S.mapM (\x -> print (fs_uri $ snd x) >> pure x)
    $ S.mapMaybeM (canonicalizing conn)
    $ S.take 30
    $ streamFilestore

reindex' :: HasCallStack => Connection -> DocId -> Filestore -> IO ()
reindex' _ _ fs = do
  -- z <- T.readFile "local-data/ssc.html"
  let Just tt = parsePermissiveTree $ decodeUtf8 $ fs_data fs
  -- print (fs_uri fs)
  -- putStrLn $ debugForest tt
  -- print $ fmap trimTree tt
  print $ scrape rankContent tt

trimTree :: TagTree T.Text -> TagTree T.Text
trimTree (TagBranch txt x0 _) = TagBranch txt x0 []
trimTree z = z


canonicalizing :: Connection -> Filestore -> IO (Maybe (DocId, Filestore))
canonicalizing conn fs = do
  -- let uri = fs_uri fs
  pure $ Just (undefined, fs)
  -- uri'  <- runRanker (Env uri conn) (decodeUtf8 $ fs_data fs) canonical
  -- runMaybeT $ do
  --   uri'' <- MaybeT $ determineHttpsAvailability $ fromMaybe uri uri'
  --   doc <- liftIO $ getDocByCanonicalUri conn uri''
  --   let did = either id d_docId doc
  --   pure (did, fs { fs_uri = uri })
