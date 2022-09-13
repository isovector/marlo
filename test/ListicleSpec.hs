module ListicleSpec where

import           Control.Exception
import           Data.Bool
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text as T
import           Signals.Listicle
import           System.Directory
import           System.FilePath
import           Test.Hspec
import           Utils
import           Utils (runScraper)


spec :: Spec
spec = do
  yes <- runIO $ getDirectory "test/data/listicle/yes"
  for_ yes $ checkIfListicle True

  no <- runIO $ getDirectory "test/data/listicle/no"
  for_ no $ checkIfListicle False


getDirectory :: FilePath -> IO [FilePath]
getDirectory dir = fmap (fmap (dir </>)) $ listDirectory dir


checkIfListicle :: Bool -> FilePath -> Spec
checkIfListicle is fp = do
  it (fp <> " is " <> bool "not " "" is <> "a listicle") $ do
    fc <- readFile fp
    z <- evaluate $ runScraper (fromJust $ parsePermissiveTree $ T.pack fc) isListicle
    shouldBe z $ Just is

