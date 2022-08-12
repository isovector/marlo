module Domains where

import           Control.Monad (join)
import           DB
import           Data.Bifunctor (bimap, first)
import           Data.Function (on)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Integration.Alexa (getGlobalRank')
import           Marlo.Robots
import           Network.URI
import           Rel8
import           Types
import           Utils (hush)


getDomain :: Connection -> URI -> IO (DomainId, RobotDirectives)
getDomain conn uri = do
  let domname =
        T.pack $ show $ uri
          { uriFragment = ""
          , uriQuery = ""
          , uriPath = ""
          , uriScheme = "https:"
          }
  Right mdom <-
    doSelect conn $ do
      dom <- each domainsSchema
      where_ $ dom_domain dom ==. lit domname
      pure (dom_id dom, dom_rules dom)

  case listToMaybe mdom of
    Just di -> pure di
    Nothing -> do
      fmap (first $ either id id) $
        rerankPopularity conn uri domname


rerankPopularity
    :: Connection
    -> URI
    -> Text
    -> IO (Either DomainId DomainId, RobotDirectives)
rerankPopularity conn uri domname = do
  rules <- fetchRobotDirectives uri
  erank <- getGlobalRank' domname
  let rank = join $ hush erank

  Right [x] <-
    doInsert conn $ upsertDomain $
      (lit emptyDomain)
        { dom_domain = lit domname
        , dom_rules = lit rules
        , dom_rank   = lit $ fmap fromIntegral rank
        }
  pure
    $ (, rules)
    $ bimap (const x) (const x) erank


upsertDomain :: Domain Expr -> Insert [DomainId]
upsertDomain dom = Insert
    { into = domainsSchema
    , rows = do
        domid <- nextDomainId
        pure $ dom
          { dom_id = domid
          }
    , onConflict = DoUpdate $ Upsert
        { index = dom_domain
        , set = \new old -> old { dom_rank = dom_rank new }
        , updateWhere = on (==.) dom_domain
        }
    , returning = Projection dom_id
    }

