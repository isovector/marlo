module Marlo.Robots
  ( fetchRobotDirectives
  , checkRobotsDirectives
  , RobotDirectives
  , RobotCheck (..)
  ) where

import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Marlo.Manager
import           Network.HTTP.Robots
import           Network.URI
import           Types
import           Utils (downloadBody)
import Control.Exception (catch, SomeException (SomeException))


fetchRobotDirectives :: URI -> IO RobotDirectives
fetchRobotDirectives uri = do
  let robotstxt = uri { uriPath = "/robots.txt" }
  catch ( do
    dl <- downloadBody $ show robotstxt
    case parseRobots $ d_body dl of
      Left _ -> pure mempty
      Right (robots, _) ->
        pure $ flip foldMap robots $ \(agent, dirs) ->
          case isApplicable agent of
            False -> mempty
            True -> foldMap applyDir dirs
          )
    $ \(SomeException _) -> pure mempty


applyDir :: Directive -> RobotDirectives
applyDir (Allow bs)    = mempty { rb_allow    = pure $ decodeUtf8 bs }
applyDir (Disallow bs) = mempty { rb_disallow = pure $ decodeUtf8 bs }
applyDir _ = mempty


isApplicable :: [UserAgent] -> Bool
isApplicable = getAny . foldMap (
  Any . \case
    Wildcard -> True
    Literal bs -> bs == marloUserAgent
                                )


data RobotCheck
  = CanIndex URI
  deriving (Eq, Ord, Show)


checkRobotsDirectives :: RobotDirectives -> RobotCheck -> Bool
checkRobotsDirectives (RobotDirectives allow deny) (CanIndex uri) = do
  let p = T.pack $ uriPath uri
      check = any @[] (flip T.isPrefixOf p)
  check allow || not (check deny)

