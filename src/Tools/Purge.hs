module Tools.Purge where

import           DB
import qualified Data.Text as T
import           Rel8
import           Signals.AcceptableURI (forbidPaths, forbidSites)


main :: IO ()
main = do
  Right conn <- connect
  Right n <- doDelete conn $ Delete
    { from = documentSchema
    , using = pure ()
    , deleteWhere = \_ d -> do
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

