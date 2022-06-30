{-# LANGUAGE OverloadedStrings #-}

module Keywords where

import           Data.Char (isLetter)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.URI
import           Text.HTML.Scalpel
import           Types


keywordify :: Text -> Maybe Text
keywordify = elim . T.strip . T.filter isLetter . T.toLower
  where
    elim "" = Nothing
    elim "a" = Nothing
    elim "and" = Nothing
    elim "are" = Nothing
    elim "as" = Nothing
    elim "at" = Nothing
    elim "be" = Nothing
    elim "been" = Nothing
    elim "but" = Nothing
    elim "by" = Nothing
    elim "for" = Nothing
    elim "from" = Nothing
    elim "have" = Nothing
    elim "i" = Nothing
    elim "in" = Nothing
    elim "is" = Nothing
    elim "it" = Nothing
    elim "its" = Nothing
    elim "lot" = Nothing
    elim "many" = Nothing
    elim "my" = Nothing
    elim "not" = Nothing
    elim "of" = Nothing
    elim "on" = Nothing
    elim "or" = Nothing
    elim "so" = Nothing
    elim "that" = Nothing
    elim "the" = Nothing
    elim "their" = Nothing
    elim "there" = Nothing
    elim "they" = Nothing
    elim "this" = Nothing
    elim "to" = Nothing
    elim "very" = Nothing
    elim "was" = Nothing
    elim "were" = Nothing
    elim "when" = Nothing
    elim "where" = Nothing
    elim "which" = Nothing
    elim "with" = Nothing
    elim "you" = Nothing
    elim "your" = Nothing
    elim x = Just x


uriKeywords :: URI -> [Text]
uriKeywords = mapMaybe keywordify . T.split (\x -> x ==  '/' || x == '-') . T.pack . uriPath


posWords :: Ranker [(Int, Text)]
posWords = do
  w <- texts "p"
  pure $ mapMaybe (traverse keywordify) $ zip [0..] $ T.words $ T.concat w

