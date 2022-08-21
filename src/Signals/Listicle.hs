module Signals.Listicle where

import           Data.Char (isDigit)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.HTML.Scalpel
import           Text.Read (readMaybe)
import           Types
import           Utils


isListicle :: Ranker Bool
isListicle = fmap (any isListicleFor) $ sequenceA
  [ texts "h1"
  , texts "h2"
  , texts "h3"
  , texts "h4"
  , texts "h5"
  ]


-- Check if each heading is numbered, evenly-spaced.
isListicleFor :: [Text] -> Bool
isListicleFor ts = do
  let nums
        = mapMaybe
            ( listToMaybe
            . mapMaybe (readMaybe @Int . T.unpack . T.filter isDigit)
            . titleSegs
            ) ts
      diffs = zipWith (-) nums $ drop 1 nums
      full_size = length nums
      size = length $ nubOrd nums
      same_size = full_size - size
  and
    [ -- At least 4 numbered items
      size >= 4
      -- that are evenly spaced, plus or minus a few skips
    , S.size (S.fromList diffs) <= min 2 (max 1 $ size - 2)
      -- that don't duplicate too many numbers
    , same_size <= 2
    ]

