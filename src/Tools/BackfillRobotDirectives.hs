module Tools.BackfillRobotDirectives where

import           DB
import           Data.Foldable (for_)
import qualified Data.Text as T
import           Marlo.Robots
import           Network.URI (parseURI)
import           Rel8


main :: IO ()
main = do
  Right conn <- connect
  Right doms <- doSelect conn $ do
    d <- each domainsSchema
    where_ $ dom_rules d ==. lit mempty
    pure $ (dom_id d, dom_domain d)
  for_ doms $ \(domid, domain) -> do
    putStrLn $ T.unpack domain
    case parseURI $ T.unpack domain of
      Nothing -> pure ()
      Just uri -> do
        rules <- fetchRobotDirectives uri
        print rules
        Right _ <- doUpdate conn $ Update
          { target = domainsSchema
          , from = pure ()
          , set = const $ \d -> d { dom_rules = lit rules }
          , updateWhere = const $ \d -> dom_id d ==. lit domid
          , returning = pure ()
          }
        pure ()

