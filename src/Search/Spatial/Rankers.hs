{-# LANGUAGE NumDecimals #-}

module Search.Spatial.Rankers where

import           Control.Applicative (liftA2)
import           DB
import           Data.List (sortOn)
import qualified Data.Map as M
import           Types


type Scorer = ([SearchResult Identity] -> [Maybe Float])

rankByStratifiedAssets :: Integer -> Maybe Float
rankByStratifiedAssets
  = stratify (/) [100, 1024, 65535, 126000000]


rankByJavascript :: [SearchResult Identity] -> [Maybe Float]
rankByJavascript
  = fmap
  $ rankByStratifiedAssets
  . fromIntegral
  . ps_js
  . sr_stats


rankByCss :: [SearchResult Identity] -> [Maybe Float]
rankByCss
  = fmap
  $ rankByStratifiedAssets
  . fromIntegral
  . ps_css
  . sr_stats


rankBySize :: [SearchResult Identity] -> [Maybe Float]
rankBySize
  = fmap
  $ stratify (/) [5000, 15000, 40000, 500000]
  . fromIntegral
  . sr_size


rankByAssetSize :: [SearchResult Identity] -> [Maybe Float]
rankByAssetSize
  = fmap
  $ rankByStratifiedAssets
  . fromIntegral
  . (liftA2 (+) ps_css ps_js)
  . sr_stats


rankByRank :: [SearchResult Identity] -> [Maybe Float]
rankByRank
  = fmap Just
  . globalUniformRanking
  . fmap sr_ranking

globalUniformRanking :: Ord a => [a] -> [Float]
globalUniformRanking [_] = [1]
globalUniformRanking sc = do
  let
      n = length sc
      sz = 1 / fromIntegral (length sc - 1)
      f = M.fromList
      ordered
        = f
        $ flip zip [id @Int 0..]
        $ fmap fst
        $ sortOn snd
        $ zip [id @Int 0 ..] sc
      getx a = ordered M.! a
  fmap ((* sz) . fromIntegral . getx) [0 .. n - 1]


rankByPopularity :: [SearchResult Identity] -> [Maybe Float]
rankByPopularity
  = fmap
  $ stratify (/) [1000, 50000, 1e7, 1e8]
  . maybe 9e7 fromIntegral
  . sr_popularity


stratify :: (Float -> Float -> Float) -> [Integer] -> Integer -> Maybe Float
stratify f ts' x = go 0 0 ts'
  where
    n :: Float
    n = 1 / (fromIntegral $ length ts')

    go  :: Float -> Integer -> [Integer] -> Maybe Float
    go _ _ [] = Nothing
    go z acc (t : ts)
      | x < t =
        let normalized = x - acc
            range = t - acc
         in Just $ f (fromIntegral normalized) (fromIntegral range) * n + z
      | otherwise = go (z + n) t ts


compileDimension :: SearchDimension -> Scorer
compileDimension ByJavascript = rankByJavascript
compileDimension ByCss        = rankByCss
compileDimension ByAssetSize  = rankByAssetSize
compileDimension ByWordCount  = rankBySize
compileDimension ByRelevance  = rankByRank
compileDimension ByPopularity = rankByPopularity

