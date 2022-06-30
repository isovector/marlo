{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Utils where

import           Control.Applicative ((<|>))
import           Control.Monad.Reader
import qualified Data.Map.Monoidal as M
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.URI
import           Text.HTML.Scalpel
import Types


runRanker :: URI -> Text -> Ranker a -> Maybe a
runRanker u t = flip runReader (Env u) . scrapeStringLikeT t


invertMap :: URI -> Set Text -> InverseIndex
invertMap u t = InverseIndex $ M.fromList $ fmap (, [u]) $ S.toList t


findDocs :: InverseIndex -> Text -> [URI]
findDocs (InverseIndex c) kw = fromMaybe [] $ M.lookup kw c


countOf :: Selector -> Ranker Int
countOf sel = fmap length $ chroots sel $ pure ()


tagClass :: String -> String -> Selector
tagClass a b = TagString a @: [hasClass b]


classOf :: String -> Selector
classOf c = AnyTag @: [hasClass c]


unsafeURI :: String -> URI
unsafeURI = fromJust . parseURI


currentURI :: Ranker URI
currentURI = asks e_uri


withClass :: Selector -> (Text -> Bool) -> Ranker ()
withClass s f = do
  cls <- T.words <$> attr "class" s
  guard $ any f cls


has :: Ranker a -> Ranker Bool
has r = True <$ r <|> pure False


posKeywordsToInv :: [(Int, Text)] -> Set Text
posKeywordsToInv = S.fromList . fmap snd

