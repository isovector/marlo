module Lib where

import           Data.Foldable (fold, for_)
import           Data.Functor ((<&>))
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Keywords
import           Network.URI
import           Types
import           Utils
import Signals (rankStuff)
import Ranking (rank)


good :: IO [(URI, Text)]
good = do
  a <- T.readFile "data/good/lw.html"
  b <- T.readFile "data/good/mr.html"
  c <- T.readFile "data/good/sandy.html"
  d <- T.readFile "data/good/moloch.html"
  pure [ (unsafeURI "https://lesswrong.com", a)
       , (unsafeURI "https://marginalrevolution.com", b)
       , (unsafeURI "https://sandy.com", c)
       , (unsafeURI "https://ssc.com", d)
       ]


bad :: IO [(URI, Text)]
bad = do
  a <- T.readFile "data/bad/travel.html"
  b <- T.readFile "data/bad/recipe.html"
  c <- T.readFile "data/bad/buzzfeed.html"
  pure [ (unsafeURI "https://croatia.com", a)
       , (unsafeURI "https://www.thechunkychef.com/family-favorite-baked-mac-and-cheese/", b)
       , (unsafeURI "https://buzzfeed.com", c)
       ]


-- main :: IO ()
-- main = do
--   let r = posWords

--   putStrLn "GOOD"
--   goods <- good
--   bads <- bad
--   for_ goods $ \(uri, txt) -> print $ runRanker uri txt rankStuff
--   putStrLn "BAD"
--   for_ bads $ \(uri, txt) -> print $ runRanker uri txt rankStuff

--   let z = goods <&> \(uri, txt) ->
--             fmap (invertMap uri . posKeywordsToInv) $ runRanker uri txt posWords
--   let x = bads  <&> \(uri, txt) ->
--             fmap (invertMap uri . posKeywordsToInv) $ runRanker uri txt posWords
--   pure $ fold $ catMaybes $ z <> x

