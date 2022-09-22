{-# LANGUAGE ApplicativeDo #-}

module Signals.Schema where

import Data.Aeson (decode, FromJSON)
import Data.ByteString.Lazy (fromStrict)
import Data.List (isInfixOf)
import Data.SchemaOrg
import Data.Text.Encoding (encodeUtf8)
import Network.URI
import Rel8.StateMask (flag, BitMask)
import Types
import Lasercutter.HTML


ldJson :: FromJSON a => HtmlParser [a]
ldJson = do
  ds <- match ("script" /\ (Just "application/ld+json" ==) . getAttr "type") text
  pure $ mapMaybe (decode . fromStrict . encodeUtf8) ds


hasPaywall :: HtmlParser Bool
hasPaywall = do
  ds <- match ("script" /\ (Just "application/ld+json" ==) . getAttr "type") text
  pure $ flip any ds $ \d ->
    case decode $ fromStrict $ encodeUtf8 d of
      Nothing -> False
      Just (IsAccessibleForFree b) -> not b


canBeFilteredOutBySchemaType :: URI -> HtmlParser (BitMask DocumentFlag)
canBeFilteredOutBySchemaType uri = do
  -- HACK: Substack and medium stupidly tag themselves as a news article
  let is_substack = maybe False (isInfixOf "substack.com" . uriRegName) $ uriAuthority uri
  let is_medium = maybe False (isInfixOf "medium.com" . uriRegName) $ uriAuthority uri

  ds <- match ("script" /\ (Just "application/ld+json" ==) . getAttr "type") text
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
