module Tools.Purge where

import DB
import Rel8
import Rel8.StateMask (flag)
import Spider (quickAcceptableDBUri)
import Types (DocumentFlag(IsProhibitedURI))


main :: IO ()
main = do
  Right conn <- connect
  Right n <- doUpdate conn $ Update
    { from =  pure ()
    , target = documentSchema
    , set = const $ \d -> d { d_flags = d_flags d <>. lit (flag IsProhibitedURI) }
    , updateWhere = \_ -> not_ . quickAcceptableDBUri . d_uri
    , returning = NumberOfRowsAffected
    }
  putStrLn $ "pruned " <> show n <> " documents rows by uri"

  Right m <- doUpdate conn $ Update
    { from =  pure ()
    , target = discoverySchema
    , set = const $ \d -> d { disc_dead = lit True }
    , updateWhere = \_ -> not_ . quickAcceptableDBUri . disc_uri
    , returning = NumberOfRowsAffected
    }
  putStrLn $ "killed " <> show m <> " discoveries by uri"

