{-# LANGUAGE NumDecimals      #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE MultiWayIf #-}

module Search.Spatial () where

import           Control.Applicative (liftA2, ZipList (ZipList), getZipList, liftA3)
import           Control.Arrow (first, second)
import           Control.Exception
import           Control.Lens (view, (<&>), (%~), _2, (*~))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (evalState, gets, modify, when)
import           DB
import           Data.Bool (bool)
import           Data.Foldable (traverse_, forM_, for_, fold)
import           Data.Function (fix, (&))
import           Data.Generics.Labels ()
import           Data.List (sortOn, foldl', sort)
import           Data.List.Extra (chunksOf)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe, maybeToList, isJust)
import           Data.Monoid
import           Data.Ord (Down(Down))
import           Data.QuadAreaTree
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           GHC.Generics (Generic)
import           Linear hiding (trace)
import qualified Lucid as L
import           Lucid.Base (makeAttribute)
import           Network.URI (uriPath)
import           Rel8 hiding (or, Enum, sum, filter, bool, evaluate, max, index)
import           Search.Compiler (compileQuery)
import           Search.DoSearch (debugSearch)
import           Search.Machinery
import           Search.Traditional (getSnippet')
import           Servant.Server.Generic ()
import           Servant.StreamingUtil (yield)
import           Types
import           Utils (unsafeURI)
import Debug.Trace (trace)
import Control.Lens.Combinators (Lens')
import Control.Concurrent (threadDelay)
import qualified Data.DList as DL


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
  type SearchMethodResult 'Spatial = QuadTree (First (SizedRegionData (SearchResult Identity)))
  limitStrategy = Limit 500
  accumResults _ ws _
    = evaluate
    . algorithm ws
    -- . fmap fst
    -- . uncurry pizzaDoughLayout
    -- . sortByCenterOffset
    -- . (\es -> fmap (fmap $ toScreenCoords ws $ length es) es)
    -- . id -- normalizeScores
    -- . fmap (id &&& score)
    -- . fmap (\sr -> sr { sr_title = correctTitle sr })

  -- accumResults _ _ docs = do
  --   let best = maximum $ fmap sr_ranking docs
  --   let rs = fmap makeRect
  --          $ fmap (\x -> x { sr_ranking = best - sr_ranking x })
  --          $ filter (not . T.null . T.strip . sr_title) docs
  --   evaluate $ foldr place (makeTree (Region 0 0 150 34) Nothing) rs
  showResults conn q qt = do
    let as = S.toList
           $ S.fromList
           $ fmap snd
           $ mapMaybe (traverse getFirst)
           $ tile qt
    for_ as $ \a -> do
      yield $ spaceResult' a
      liftIO $ threadDelay 5e4
    let cdids = chunksOf 20 $ fmap (sr_id . srd_data) as
        q' = compileQuery q
    forM_ cdids $ \dids -> do
      liftIO $ print dids
      msnips <-
        liftIO $ doSelect conn $ do
          d <- each documentSchema
          where_ $ in_ (d_docId d) $ fmap lit $ dids
          fmap (d_docId d, d_uri d,) $ getSnippet' d q'
      case msnips of
        Left z -> do
          liftIO $ print z
          pure ()
        Right snips -> do
          yield $
            for_ snips $ \(did, uri, snip) -> do
              L.div_
                  [ L.id_ $ "snip" <> T.pack (show $ unDocId did)
                  , L.class_ "spacesnip"
                  ] $ do
                L.span_ [L.class_ "url"] $ L.a_ [L.href_ uri] $ L.toHtml uri
                L.p_ [L.class_ "snippet"] $ L.toHtmlRaw snip




    -- . fmap (fmap r_data)
    --
  debugResults = undefined
    -- = putStrLn
    -- . showTree (maybe ' ' (const 'X'))


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


type Scorer = ([SearchResult Identity] -> [Maybe Float])

scoreParam
  :: Scorer
  -> Scorer
  -> Scorer
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


algorithm
    :: WindowSize
    -> [SearchResult Identity]
    -> QuadTree (First (SizedRegionData (SearchResult Identity)))
algorithm ws srs = do
  let n = fromIntegral $ length srs
  tighten (isJust . getFirst)
    . foldr
        doPlace
        (makeTree $ Region 0 (-10000) (ws_width ws) 20000)
    . fmap (uncurry $ buildRegion ws)
    . renormalizeY n
    . sortByCenterOffset (_2 . _xy) (_y)
    . scoreParam rankByPopularity rankByPageSize rankByPopularity
    . fmap (\sr -> sr { sr_title = correctTitle sr })
    $ srs


traceShowF :: Show a1 => (a2 -> a1) -> a2 -> a2
traceShowF f a = trace (show $ f a) a


renormalizeY
    :: Float
    -> [(SearchResult Identity, V3 Float)]
    -> [(SearchResult Identity, V3 Float)]
renormalizeY n xs = do
  let lo = minimum $ fmap (view _y . snd) xs
      hi = maximum $ fmap (view _y . snd) xs
      z = hi - lo
  fmap (second $ _y %~ \y -> (y - lo) / z * 1080) xs


  -- _2 . _y *~
  --   view _y (measureText' (denormalizePt 0.5) "x") * n


toScreenCoords :: WindowSize -> Int -> V2 Int -> V2 Float
toScreenCoords (WindowSize w h) n (V2 x y) =
  V2 (fromIntegral x / fromIntegral n * fromIntegral w)
     (fromIntegral y)



sortByCenterOffset
    :: (Ord b, Ord c, Num c, Fractional b, Show c)
    => Lens' a (V2 b)  -- ^ get a V2 out of the data
    -> Lens' (V2 b) c     -- ^ what to sort the V2 on
    -> [a]
    -> ([a])
sortByCenterOffset l f placed = do
  let
      n = length placed
      midpoint = (!! div n 2)
               $ sort
               $ fmap (view $ l . f) placed
  sortOn (abs . subtract midpoint . view (l . f))
    $ trace (show midpoint) $ placed


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
            , T.pack $ show $ 250 + y
            , "; "
            , "left: "
            , T.pack $ show $ x
            , ";"
            ]
        ] $ do
      let uri = unsafeURI $ T.unpack $ sr_uri d
      L.img_
        [ L.src_ $ T.pack $ show $ uri { uriPath = "/favicon.ico" }
        , L.width_  (T.pack $ show $ view _x $ favSize @Int)
        , L.height_ (T.pack $ show $ view _y $ favSize @Int)
        ]
      L.a_ [ L.href_ $ sr_uri d
           , makeAttribute "data-docid" $ T.pack $ show $ unDocId $ sr_id d
           , L.onmouseover_ "tt(this)"
           , L.onmouseout_  "untt(this)"
           ] $ L.toHtml title

spaceResult' :: SizedRegionData (SearchResult Identity) -> L.Html ()
spaceResult' (SizedRegionData pt (Region x y _ _) d) = do
  let title = T.strip $ sr_title d
  when (not $ T.null title) $ do
    L.span_
        [ L.class_ "spatial-title"
        , L.style_ $ mconcat
            [ "position: absolute;"
            , "top: "
            , T.pack $ show $ 250 + y
            , "; "
            , "left: "
            , T.pack $ show $ x
            , "; "
            , "font-size: "
            , T.pack $ show pt
            , "pt;"
            ]
        ] $ do
      let uri = unsafeURI $ T.unpack $ sr_uri d
      L.img_
        [ L.src_ $ T.pack $ show $ uri { uriPath = "/favicon.ico" }
        , L.width_  (T.pack $ show $ view _x $ favSize @Int)
        , L.height_ (T.pack $ show $ view _y $ favSize @Int)
        ]
      L.a_ [ L.href_ $ sr_uri d
           , makeAttribute "data-docid" $ T.pack $ show $ unDocId $ sr_id d
           , L.onmouseover_ "tt(this)"
           , L.onmouseout_  "untt(this)"
           ] $ L.toHtml title


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

-- test
--   :: Scorer
--   -> Scorer
--   -> Scorer
--   -> [SearchResult Identity]
--   -> QuadTree (First (SearchResult Identity))
-- test scorex scorey scoresz
--   = undefined
--   . fmap (uncurry buildRegion)
--   . scoreParam scorex scorey scoresz
--   . fmap (\sr -> sr { sr_title = correctTitle sr })


data SizedRegionData a = SizedRegionData
  { srd_size   :: Float  -- in pts
  , srd_region :: Region
  , srd_data   :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)

buildRegion
    :: WindowSize
    -> SearchResult Identity
    -> V3 Float
    -> SizedRegionData (SearchResult Identity)
buildRegion (WindowSize ww _) sr (V3 x y prept) =
  let pt = denormalizePt prept
      V2 w h = fmap round
             $ extendForIcon
             $ measureText' pt
             $ sr_title sr
   in SizedRegionData
        pt
        (Region (fitIn ww w $ round $ x * fromIntegral ww) (round y) w h)
        sr


denormalizePt :: Float -> Float
denormalizePt prept = 10 + prept * 3


doPlace
    :: forall a
     . SizedRegionData a
    -> QuadTree (First (SizedRegionData a))
    -> QuadTree (First (SizedRegionData a))
doPlace srd0 qt = go 0 srd0
  where
    go :: Int -> SizedRegionData a -> QuadTree (First (SizedRegionData a))
    go 3 _ = qt
    go n srd = do
      let r = srd_region srd
      case fmap fst
         $ DL.toList
         $ hitTestR (curry $ traverse $ DL.fromList . maybeToList . getFirst)
                    r qt of
        [] -> fill (First $ Just srd) r qt
        conflicts -> do
          let sides = foldMap (checkConflicts r) conflicts
          case (canPan sides, canTryResize r conflicts) of
            (True, _) ->
              go (n + 1) $ srd & #srd_region %~ flip pan conflicts
            (_, True) -> trace "wants a resize" qt
            (_, _) -> qt

pan :: Region -> [Region] -> Region
pan r0 = flip foldl' r0 $ \r con ->
  case getIntersection r con of
    Just c -> offsetByY c $ offsetByX c r
    Nothing -> r

offsetByY :: Region -> Region -> Region
offsetByY (Region _ cy _ ch) r@(Region rx ry rw rh)
  | ry      == cy      = Region rx (ry + ch) rw rh
  | ry + rh == cy + ch = Region rx (ry - ch) rw rh
  | otherwise = r

offsetByX :: Region -> Region -> Region
offsetByX (Region cx _ cw _) r@(Region rx ry rw rh)
  | rx      == cx      = Region (rx + cw) ry rw rh
  | rx + rw == cx + cw = Region (rx - cw) ry rw rh
  | otherwise = r

checkConflicts :: Region -> Region -> Set SideOf
checkConflicts rel@(Region rx ry rw rh) con =
  case getIntersection rel con of
    Just (Region cx cy cw ch) -> S.fromList $
      mconcat
        [ [ LeftSide   | rx      == cx      ]
        , [ RightSide  | rx + rw == cx + cw ]
        , [ TopSide    | ry      == cy      ]
        , [ BottomSide | ry + rh == cy + ch ]
        ]
    Nothing       -> mempty

canTryResize :: Region -> [Region] -> Bool
canTryResize r r2 = r /= fold r2

canPan :: Set SideOf -> Bool
canPan s = not $ or
  [ S.member LeftSide s && S.member RightSide  s
  , S.member TopSide  s && S.member BottomSide s
  ]

data SideOf = LeftSide | RightSide | TopSide | BottomSide
  deriving (Eq, Ord, Show, Enum, Bounded)



fitIn :: Int -> Int -> Int -> Int
fitIn _sw _w x = x


plop :: SearchResult Identity -> V2 Float
plop = r_pos . makeRect


-- determined experimentally
measureText' :: Float -> Text -> V2 Float
measureText' pt t = V2 (8 * fromIntegral (T.length t)) 18 ^* (pt / 10)

-- icons are square and fill the entire lineheight, so extend the width by that
extendForIcon :: V2 Float -> V2 Float
extendForIcon (V2 x y) = V2 (x + y) y

