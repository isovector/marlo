-- Sorry for the awful module; I stole it from graph-utils which no longer
-- compiles.
{-# LANGUAGE BangPatterns #-}

module Data.Graph.PageRank (pageRanks) where

import Control.Monad.RWS (RWS(..), asks, gets, execRWS, get, put)
import Data.Graph.Inductive hiding (size)
import Prelude hiding (lookup)
import Data.Map (Map, lookup, fromList, foldrWithKey, findWithDefault)
import Data.Maybe (fromJust)
import Control.Monad
import Debug.Trace (traceM)

data Env = Env {node :: [Node], size :: Int, from :: Map Node [Node], outdegrees :: Map Node Int}

-- |'RankDic' is the Map for holding PageRank data.
type PRMachine = RWS Env () (Map Node Double)

lookupEnv :: (Ord a) => (Env -> Map a b) -> a -> PRMachine b
lookupEnv f a = do{ dic<-asks f; pure $ fromJust $ lookup a dic}

outdegree :: Node -> PRMachine Int
outdegree = lookupEnv outdegrees

froms :: Node -> PRMachine [Node]
froms = lookupEnv from

currentRank :: Node -> PRMachine Double
currentRank nd = gets (fromJust.lookup nd)

-- |'pageRanks' calculate the PageRank for each node in the Graph 'gr'
pageRanks :: Graph gr => gr a b -> Double -> Double -> Map Node Double
pageRanks gr epsilon error = fst $ execRWS (steps 0) Env{node=nds, size=count, from=froms, outdegrees=outdegs} initRanks
    where nds = nodes gr
          count :: (Num a) => a
          count = fromIntegral $ noNodes gr
          froms = fromList $ zip nds $ fmap (pre gr) nds
          outdegs = fromList $ zip nds $ fmap (outdeg gr) nds
          initRanks = fromList $ zip nds $ replicate count (1/count)
          steps 1000 = get
          steps n = do
            !_ <- traceM $ "stepping " <> show n
            old <- get
            new <- calcPageRank epsilon
            let cond = foldrWithKey (\k a b -> b && ((findWithDefault (1/0) k new)-a < error)) True old
            if cond then pure new else steps (n + 1)



calcPageRank :: Double -> PRMachine (Map Node Double)
calcPageRank epsilon = do
  nds <- asks node
  count <- asks $ fromIntegral . size
  dic <- forM nds $ \n -> do
                 frms <- froms n
                 ranks <- forM frms $ \m -> do
                            deg <- outdegree m
                            rank <- currentRank m
                            pure (rank/fromIntegral deg)
                 pure (n, epsilon/count + (1-epsilon)*(sum ranks))
  let rdic = fromList dic
  put rdic
  pure rdic

