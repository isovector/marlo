module DB.RootSites where

import Rel8
import Data.Text (Text)
import Data.Int (Int16)
import Rel8.Arrays (arrayFill)


rootSites :: [Expr Text]
rootSites =
  -- Due to uri normalization, it's important to not have a trailing slash on
  -- these
  [ "https://slatestarcodex.com"  -- rationality / econ
  , "https://jeremykun.com"       -- math
  , "https://neocities.org"       -- amateur
  , "https://mitxela.com"         -- diy
  , "https://seirdy.one"          -- search engines
  , "https://coffeeadastra.com"   -- coffee
  , "https://spencermounta.in"    -- ?
  , "http://worrydream.com"       -- ?
  ]


numRootSites :: Int
numRootSites = length rootSites


nullDist :: Expr [Maybe Int16]
nullDist = arrayFill (lit $ fromIntegral numRootSites) Rel8.null

