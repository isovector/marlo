module Signals.Listicle where

import           Control.Monad (join)
import           Data.Char (isDigit)
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Lasercutter.HTML
import           Text.Read (readMaybe)


isListicle :: HtmlParser Bool
isListicle
  = fmap (any isListicleFor)
  $ sequenceA
  $ fmap (fmap join)
      [ target "h1" texts
      , target "h2" texts
      , target "h3" texts
      ]


-- Check if each heading is numbered, evenly-spaced.
isListicleFor :: [Text] -> Bool
isListicleFor ts = do
  let nums
        = mapMaybe (readMaybe @Float . T.unpack)
        $ fmap appendTrailingZero
        $ fmap (T.takeWhile (\x -> isDigit x || x == '.') . T.strip)
        $ ts
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
      -- where each heading changes by at most one
    , abs (head diffs) == 1
      -- and the numbers are probably not years
    , round @_ @Int (minimum nums) <= 1600
    ]

appendTrailingZero :: Text -> Text
appendTrailingZero t =
  case T.takeEnd 1 t == "." of
    False -> t
    True -> t <> "0"

