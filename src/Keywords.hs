module Keywords where

import           Data.Char (GeneralCategory (..), generalCategory)

isKeywordLetter :: Char -> Bool
isKeywordLetter c =
  any (== generalCategory c)
    [ UppercaseLetter
    , LowercaseLetter
    , TitlecaseLetter
    ]

