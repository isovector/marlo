module Tools.Purge where

import           DB
import qualified Data.Text as T
import           Rel8
import           Rel8.StateMask (flag)
import           Signals.AcceptableURI (forbidPaths, forbidSites)
import           Types (DocumentFlag(IsProhibitedURI))


main :: IO ()
main = do
  Right conn <- connect
  Right n <- doUpdate conn $ Update
    { from =  pure ()
    , target = documentSchema
    , set = const $ \d -> d { d_flags = d_flags d <>. lit (flag IsProhibitedURI) }
    , updateWhere = \_ d -> do
        let paths =
              foldr1 (||.) $ do
                z <- forbidPaths
                pure $ like (lit $ T.pack $ "%" <> z <> "%") $ d_uri d
            sites =
              foldr1 (||.) $ do
                z <- forbidSites
                pure $ like (lit $ T.pack $ "%" <> z <> "/%") $ d_uri d
        paths ||. sites
    , returning = NumberOfRowsAffected
    }
  putStrLn $ "deleted " <> show n <> " rows by uri"

