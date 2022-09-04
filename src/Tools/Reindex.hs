module Tools.Reindex where

import           Control.Exception
import           Control.Monad (void)
import           DB
import           Data.Foldable (for_)
import           Rel8
import qualified Streaming.Prelude as S
import           Types
import Marlo.Filestore (streamFilestore)
import Spider (reindex)


main :: IO ()
main = do
  Right conn <- connect

  S.effects
    $ S.mapM (uncurry $ reindex conn)
    $ S.mapM _
    $ streamFilestore

