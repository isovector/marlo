module Index where

import Control.Exception
import Control.Monad (void)
import DB
import Hasql.Connection (acquire)
import Hasql.Session
import Rel8
import Spider (indexFromDB)
import Data.Foldable (for_)


main :: IO ()
main = do
  -- [uri] <- getArgs
  Right conn <- acquire connectionSettings
  Right docs <- flip run conn $ statement () $ select $ do
    d <- each discoverySchema
    where_ $ d_state d ==. lit Explored -- &&. like (lit $ "%" <> T.pack uri <> "%") (d_uri d)
    pure $ d_docId d
  for_ docs $ \did -> do
    Right [doc] <-
      flip run conn $ statement () $ select $ do
        d <- each discoverySchema
        where_ $ d_docId d ==. lit did
        pure d
    print $ d_uri doc
    catch (indexFromDB conn doc) $ \(SomeException _) -> do
      putStrLn "errored ^"
      void $ flip run conn $ statement () $ update $ Update
        { target = discoverySchema
        , from = pure ()
        , set = \_ d -> d { d_state = lit NoContent }
        , updateWhere = \_ d -> d_docId d ==. lit (d_docId doc)
        , returning = pure ()
        }

