{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Search.Spatial () where

import           Control.Applicative (liftA2, ZipList (ZipList), getZipList, liftA3)
import           Control.Arrow ((&&&), first, second)
import           Control.Exception
import           Control.Lens (view, (<&>))
import           Control.Monad.State (evalState, gets, modify, when)
import           DB
import           Data.Bool (bool)
import           Data.Foldable (traverse_)
import           Data.Function (fix)
import           Data.List (sortOn)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe, isJust)
import           Data.Monoid
import           Data.Ord (Down(Down))
import           Data.QuadAreaTree
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Linear hiding (trace)
import qualified Lucid as L
import           Network.URI (uriPath)
import           Rel8 hiding (sum, filter, bool, evaluate, max, index)
import           Search.DoSearch (debugSearch)
import           Search.Machinery
import           Servant.Server.Generic ()
import           Types
import           Utils (unsafeURI)


-- parameters:
--   actual positions assigned to the nodes
--   transformation into screen space
--
-- algorithm?
--   find the midpoint of the dataset
--   order by distance from midpoint
--   build an infinte quadtree guaranteed to fit everything
--   insert in order of distance
--      break ties by sliding AWAY from the midpoint
--      biased vertically
--   renormalize the tree


instance SearchMethod 'Spatial where
  type SearchMethodResult 'Spatial = QuadTree (Maybe (SearchResult Identity))
  limitStrategy = Limit 500
  accumResults _ ws _
    = evaluate
    . undefined
    -- . newLayoutAlgorithm ws
    -- . fmap fst
    -- . uncurry pizzaDoughLayout
    -- . sortByCenterOffset
    -- . (\es -> fmap (fmap $ toScreenCoords ws $ length es) es)
    -- . id -- normalizeScores
    -- . fmap (id &&& score)
    -- . fmap (\sr -> sr { sr_title = correctTitle sr })

  showResults
    = traverse_ spaceResult
    . uniqueTiles
    . mapMaybe (traverse id)
    . tile
  debugResults
    = putStrLn
    . showTree (maybe ' ' (const 'X'))


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


rankByPageSize :: [SearchResult Identity] -> [Maybe Float]
rankByPageSize
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


scoreParam
  :: ([SearchResult Identity] -> [Maybe Float])
  -> ([SearchResult Identity] -> [Maybe Float])
  -> ([SearchResult Identity] -> [Maybe Float])
  -> [SearchResult Identity]
  -> [(SearchResult Identity, V3 Float)]
scoreParam fx fy fz srs =
  let xs = ZipList $ fx srs
      ys = ZipList $ fy srs
      zs = ZipList $ fz srs
   in catMaybes
    $ fmap sequenceA
    $ zip srs
    $ getZipList
    $ liftA3 V3
        <$> xs
        <*> ys
        <*> zs


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
  . maybe 1e9 fromIntegral
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


pizzaDoughLayout :: V2 Float -> [(SearchResult Identity, V2 Float)] -> QuadTree (Maybe (SearchResult Identity))
pizzaDoughLayout = error "not implemented"

toScreenCoords :: WindowSize -> Int -> V2 Int -> V2 Float
toScreenCoords (WindowSize w h) n (V2 x y) =
  V2 (fromIntegral x / fromIntegral n * fromIntegral w)
     (fromIntegral y)


normalizeScores :: (Ord a, Ord b) => [(a, V2 b)] -> [(a, V2 Int)]
normalizeScores sc = do
  let
      f = M.fromList
        . fmap (first fst)
        . flip zip [id @Int 0 ..]
      orderedx  = f $ sortOn (view _x . snd) sc
      orderedy  = f $ sortOn (Down . view _y . snd) sc
      getx a = orderedx M.! a
      gety a = orderedy M.! a
  fmap fst sc <&> \a -> (a,) $ V2 getx gety <*> pure a


sortByCenterOffset :: (Ord b, Fractional b) => [(a, V2 b)] -> (V2 b, [(a, V2 b)])
sortByCenterOffset placed = do
  let
      n = length placed
      midpoint  = sum (fmap snd placed) / fromIntegral n
  (midpoint,)
    $ sortOn (quadrance . snd)
    $ fmap (second $ subtract midpoint)
    $ placed


correctTitle :: SearchResult Identity -> Text
correctTitle = trimTo 37 "..." . sr_title


measureText :: Text -> V2 Int
measureText s = V2 (T.length s + 1) 1
-- plus one for the icon


score :: SearchResult Identity -> V2 Float
score sr =
  V2 (log $ log $ max 1 $ fromIntegral $ fromMaybe 1e6 $ sr_popularity sr)
     (sr_ranking sr * 10)



makeRect :: SearchResult Identity -> Rect (SearchResult Identity)
makeRect sr = Rect
  { r_pos = score sr
  , r_size = measureText title'
  , r_data = sr { sr_title = title' }
  }
  where
    title' = sr_title sr


trimTo :: Int -> Text -> Text -> Text
trimTo sz rest t
  | T.length t > sz
  = T.take sz t <> rest
  | otherwise = t



onlyIfDifferent :: Eq a => (a -> a) -> a -> Maybe a
onlyIfDifferent f a =
  let fa = f a
   in bool (Just fa) Nothing $ fa == a


uniqueTiles :: [(Region, SearchResult Identity)] -> [(Region, SearchResult Identity)]
uniqueTiles ts = flip evalState mempty $ fmap catMaybes $
  for ts $ \t@(_, sr) -> do
    gets (S.member $ sr_id sr) >>= \case
       False -> do
         modify $ S.insert $ sr_id sr
         pure $ Just t
       True -> pure Nothing


xSize :: Num a => a
xSize = 11


ySize :: Num a => a
ySize = 18


favSize :: Num a => V2 a
favSize = pure 12


spaceResult :: (Region, SearchResult Rel8.Result) -> L.Html ()
spaceResult (Region x y _ _, d) = do
  let title = T.strip $ sr_title d
  when (not $ T.null title) $ do
    L.span_
        [ L.class_ "spatial-title"
        , L.style_ $ mconcat
            [ "position: absolute;"
            , "top: "
            , T.pack $ show $ 250 + y * ySize
            , "; "
            , "left: "
            , T.pack $ show $ x
            , "%"
            ]
        ] $ do
      let uri = unsafeURI $ T.unpack $ sr_uri d
      L.img_
        [ L.src_ $ T.pack $ show $ uri { uriPath = "/favicon.ico" }
        , L.width_  (T.pack $ show $ view _x $ favSize @Int)
        , L.height_ (T.pack $ show $ view _y $ favSize @Int)
        ]
      L.a_ [L.href_ $ sr_uri d] $ L.toHtml title


main :: IO ()
main = debugSearch @'Spatial (Term "cities") 1


data Rect a = Rect
  { r_pos    :: !(V2 Float)
  , r_size   :: !(V2 Int)
  , r_data   :: !a
  }
  deriving (Eq, Ord, Show, Functor)


rectCenter :: Rect a -> V2 Float
rectCenter Rect{..} = r_pos + fmap fromIntegral r_size / 2


------------------------------------------------------------------------------
-- | Take a size and a centerpoint, get the top left corner
uncenter :: V2 Int -> V2 Float -> V2 Float
uncenter sz center = center - fmap fromIntegral sz / 2


rectToRegion :: Rect a -> Region
rectToRegion Rect{r_pos = fmap round -> V2 x y, r_size = V2 w h } = Region x y w h


fillRect :: Rect a -> QuadTree (First (Rect a)) -> QuadTree (First (Rect a))
fillRect r = fill (pure r) $ rectToRegion r


newLayoutAlgorithm
    :: WindowSize
    -> [SearchResult Identity]
    -> QuadTree (First (SearchResult Identity))
newLayoutAlgorithm (WindowSize sw _) res = do
  let n         = length res
      placed0   = fmap (id &&& plop) res
      midpoint  = sum (fmap snd placed0) / fromIntegral n
      ordered   = sortOn (quadrance . snd) $ fmap (second $ subtract midpoint) placed0
      orderedx  = M.fromList
                $ fmap (first $ sr_id . fst)
                $ flip zip [0..]
                $ sortOn (view _x . snd) placed0
      orderedy  = M.fromList
                $ fmap (first $ sr_id . fst)
                $ flip zip [0..]
                $ sortOn (Down . view _y . snd) placed0
      sz        = (^) @Int @Int 2 14
      tree0     = makeTree (Region (-sz) 0 (sz * 2) (sz * 2))
      total     = length placed0
      reordered =
        placed0 <&> \(sr, _) ->
          ( sr
          , V2 ((* fromIntegral sw) . (/ fromIntegral total))
               id <*>
              (fmap (fromIntegral @_ @Float)(V2 (orderedx M.!)
                                                (orderedy M.!) <*> pure (sr_id sr)))
          )
      tree      = foldr (uncurry place') tree0 reordered
  renormalize (\(Region _ _ w h) -> Region 0 0 w h)
    $ tighten (isJust . getFirst)
    $ fmap (fmap snd)
    $ tree


place'
    :: SearchResult Identity
    -> V2 Float
    -> QuadTree (First ((V2 Float, Region), SearchResult Identity))
    -> QuadTree (First ((V2 Float, Region), SearchResult Identity))
place' sr p0 q = do
  let V2 w h = measureText $ sr_title sr
  flip fix p0 $ \go p@(V2 x y) -> do
    let r = Region (round x) (round y) w h
    case inBounds q r of
      False -> q
      True ->
        case getFirst $ hitTest id r q of
          Just (hit, _) -> do
            let p' = p + V2 0 1
            go $ p'
          Nothing -> fill (First $ Just ((p, r), sr)) r q


plop :: SearchResult Identity -> V2 Float
plop = r_pos . makeRect

