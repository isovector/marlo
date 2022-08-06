module Index where

import Control.Exception
import Control.Monad (void)
import DB
import Rel8
import Spider (indexFromDB)
import Data.Foldable (for_)
import Types


main :: IO ()
main = do
  -- [uri] <- getArgs
  Right conn <- connect
  Right docs <- doSelect conn $ do
    d <- each documentSchema
    where_ $ d_state d ==. lit Explored -- &&. like (lit $ "%" <> T.pack uri <> "%") (d_uri d)
    pure $ d_docId d
  for_ docs $ \did -> do
    Right [doc] <-
      doSelect conn $ do
        d <- each documentSchema
        where_ $ d_docId d ==. lit did
        pure d
    print $ d_uri doc
    catch (indexFromDB conn doc) $ \(SomeException _) -> do
      putStrLn "errored ^"
      void $ doUpdate conn $ Update
        { target = documentSchema
        , from = pure ()
        , set = \_ d -> d { d_state = lit NoContent }
        , updateWhere = \_ d -> d_docId d ==. lit (d_docId doc)
        , returning = pure ()
        }

