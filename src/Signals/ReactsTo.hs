module Signals.ReactsTo where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.HTML.Scalpel
import           Types


isReactsTo :: Ranker Bool
isReactsTo = fmap (any isReactsToFor . fmap (fmap T.toLower)) $ sequenceA
  [ texts "h1"
  , texts "h2"
  , texts "h3"
  ]


isReactsToFor :: [Text] -> Bool
isReactsToFor = any $ \t ->
  any ($ t)
    [ T.isInfixOf "reacts to"
    , T.isInfixOf "react to"
    ]

