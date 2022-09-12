module Signals.Schema where

import Control.Monad.Reader
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import Data.List (isInfixOf)
import Data.SchemaOrg
import Data.Text.Encoding (encodeUtf8)
import Network.URI
import Rel8.StateMask (flag, BitMask)
import Text.HTML.Scalpel
import Types


hasPaywall :: Ranker Bool
hasPaywall = do
  ds <- texts $ "script" @: ["type" @= "application/ld+json"]
  pure $ flip any ds $ \d ->
    case decode $ fromStrict $ encodeUtf8 d of
      Nothing -> False
      Just (IsAccessibleForFree b) -> not b


canBeFilteredOutBySchemaType :: Ranker (BitMask DocumentFlag)
canBeFilteredOutBySchemaType = do
  uri <- asks $ e_uri
  -- HACK: Substack and medium stupidly tag themselves as a news article
  let is_substack = maybe False (isInfixOf "substack.com" . uriRegName) $ uriAuthority uri
  let is_medium = maybe False (isInfixOf "medium.com" . uriRegName) $ uriAuthority uri

  ds <- texts $ "script" @: ["type" @= "application/ld+json"]
  pure $ flip foldMap ds $ \d ->
    case fmap getMetadataType $ decode $ fromStrict $ encodeUtf8 d of
      Just "AggregateOffer" -> flag IsShopping
      Just "AmpStory"       -> flag IsAnticompetitiveTech
      Just "Clip"           -> flag IsMedia
      Just "Episode"        -> flag IsMedia
      Just "Movie"          -> flag IsMedia
      Just "NewsArticle"    ->
        case is_substack || is_medium of
          True -> mempty
          False -> flag IsNews
      Just "Offer"          -> flag IsShopping
      Just "Product"        -> flag IsShopping
      Just "TVSeason"       -> flag IsMedia
      Just "TVSeries"       -> flag IsMedia
      _                     -> mempty

