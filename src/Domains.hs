module Domains where

import           DB
import           Data.Bifunctor (bimap)
import           Data.Function (on)
import           Data.Functor ((<&>))
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Integration.Alexa (getGlobalRank')
import           Network.URI
import           Rel8
import           Types
import Control.Monad (join)
import Utils (hush)


getDomain :: Connection -> URI -> IO DomainId
getDomain conn uri = do
  let domname =
        T.pack $ show $ uri
          { uriFragment = ""
          , uriQuery = ""
          , uriPath = ""
          , uriScheme = "https:"
          , uriAuthority = uriAuthority uri <&> \auth -> auth
              { uriUserInfo = ""
              , uriPort = ""
              , uriRegName = dropSubdomain $ uriRegName auth
              }
          }
  Right mdom <-
    doSelect conn $ do
      dom <- each domainsSchema
      where_ $ dom_domain dom ==. lit domname
      pure $ dom_id dom

  case listToMaybe mdom of
    Just di -> pure di
    Nothing -> do
      fmap (either id id) $
        rerankPopularity conn domname


rerankPopularity :: Connection -> Text -> IO (Either DomainId DomainId)
rerankPopularity conn domname = do
  erank <- getGlobalRank' domname
  let rank = join $ hush erank

  Right [x] <- doInsert conn $ Insert
    { into = domainsSchema
    , rows = do
        domid <- nextDomainId
        pure $ Domain
          { dom_id = domid
          , dom_domain = lit domname
          , dom_rank = lit $ fmap fromIntegral rank
          }
    , onConflict = DoUpdate $ Upsert
        { index = dom_domain
        , set = \new old -> old { dom_rank = dom_rank new }
        , updateWhere = on (==.) dom_domain
        }
    , returning = Projection dom_id
    }
  pure $ bimap (const x) (const x) erank


dropSubdomain :: String -> String
dropSubdomain
  = T.unpack
  . T.intercalate "."
  . reverse
  . take 2
  . reverse
  . T.split (== '.')
  . T.pack

