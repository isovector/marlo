{-# LANGUAGE NumDecimals      #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Search.Spatial () where

import           Control.Applicative (ZipList (ZipList), getZipList, liftA3)
import           Control.Arrow (second)
import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Lens (view, (%~), lens, to, preview)
import           Control.Lens.Combinators (Lens')
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (when, evalState, modify, get)
import           DB
import qualified Data.DList as DL
import           Data.Foldable (forM_, for_, fold)
import           Data.Function ((&))
import           Data.Generics.Labels ()
import           Data.Generics.Product (position)
import           Data.Generics.Sum (_Ctor)
import           Data.List (sortOn, foldl', sort)
import           Data.List.Extra (chunksOf)
import           Data.Maybe (catMaybes, mapMaybe, maybeToList)
import           Data.Monoid
import           Data.QuadAreaTree
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Debug.Trace (trace)
import           GHC.Generics (Generic)
import           Linear hiding (trace)
import qualified Lucid as L
import           Lucid.Base (makeAttribute)
import           Network.URI (uriPath)
import           Rel8 hiding (or, Enum, sum, filter, bool, evaluate, max, index)
import           Search.Compiler (compileQuery)
import           Search.Machinery
import           Search.Spatial.Rankers
import           Search.Traditional (getSnippet')
import           Servant.Server.Generic ()
import           Servant.StreamingUtil (yield)
import           Types
import           Utils (unsafeURI)


instance SearchMethod 'Spatial where
  type SearchMethodResult 'Spatial =
    QuadTree (First (SizedRegionData (SearchResult Identity)))

  limitStrategy = Limit 500

  accumResults _ ws dims _
    = evaluate
    . algorithm ws dims

  showResults conn q qt = do
    let as = S.toList
           $ S.fromList
           $ fmap snd
           $ mapMaybe (traverse getFirst)
           $ tile qt
    for_ as $ \a -> do
      yield $ spaceResult' a
      liftIO $ threadDelay 1e4
    let cdids = chunksOf 20 $ mapMaybe (preview $ _Ctor @"SizedRegionData" . position @3 . to sr_id) as
        q' = compileQuery q
    forM_ cdids $ \dids -> do
      msnips <-
        liftIO $ doSelect conn $ do
          d <- each documentSchema'
          let t = d_table d
          where_ $ in_ (d_docId t) $ fmap lit $ dids
          fmap (d_docId t, d_uri t,) $ getSnippet' d q'
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

  debugResults = undefined


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

algorithm
    :: WindowSize
    -> V3 SearchDimension
    -> [SearchResult Identity]
    -> QuadTree (First (SizedRegionData (SearchResult Identity)))
algorithm ws (fmap compileDimension -> V3 x y z) srs = do
  let n = fromIntegral $ length srs
  foldr
        doPlace
        (startingTree ws)
    . sortByCenterOffset (#srd_region . regionPos) _y
    . tightenY
    . fmap (uncurry $ buildRegion ws)
    . renormalizeZ
    . renormalizeY n
    . scoreParam x y z
    . fmap (\sr -> sr { sr_title = correctTitle sr })
    $ srs


startingTree :: WindowSize -> QuadTree (First (SizedRegionData (SearchResult Identity)))
startingTree (WindowSize w h)
  -- = addBarrier (Region (-100) (-1000) 100 (h + 2000))
  = addBarrier (Region w (-1000) 100 (h + 2000))
  -- $ addBarrier (Region (-1000) (-100) (w + 2000) 100)
  -- $ addBarrier (Region (-1000) h (w + 2000) 100)
  $ makeTree
  $ Region (-w) (-10000) (2 * w) 20000


addBarrier
    :: Region
    -> QuadTree (First (SizedRegionData (SearchResult Identity)))
    -> QuadTree (First (SizedRegionData (SearchResult Identity)))
addBarrier r = fill (First $ Just $ Barrier r) r





regionPos :: Lens' Region (V2 Int)
regionPos = lens
  (\(Region x y _ _) -> V2 x y)
  (\(Region _ _ w h) (V2 x y) -> Region x y w h)


tightenY
    :: [SizedRegionData (SearchResult Identity)]
    -> [SizedRegionData (SearchResult Identity)]
tightenY [] = []
tightenY (sortOn (r_y . srd_region) -> xs) = do
  let bot (Region _ y _ h) = y + h
  flip evalState (r_y . srd_region $ head xs) $
    for xs $ \x -> do
      let r = srd_region x
      b <- get
      let r' = case abs (b - r_y r) <= round (ySize * 1.5) of
                 True  -> r
                 False -> r { r_y = b }
      modify $ max $ bot r'
      pure $ x { srd_region = r' }


-- TODO(sandy): magic number height
renormalizeY
    :: Float
    -> [(SearchResult Identity, V3 Float)]
    -> [(SearchResult Identity, V3 Float)]
renormalizeY n xs = do
  let lo = minimum $ fmap (view _y . snd) xs
      hi = maximum $ fmap (view _y . snd) xs
      z = hi - lo
  fmap (second $ _y %~ \y -> (y - lo) / z * 1080) xs

renormalizeZ
    :: [(SearchResult Identity, V3 Float)]
    -> [(SearchResult Identity, V3 Float)]
renormalizeZ zs = do
  let lo = minimum $ fmap (view _z . snd) zs
      hi = maximum $ fmap (view _z . snd) zs
      rng = hi - lo
  flip fmap zs $ second $ _z %~ \z ->
    (z - lo) / rng


sortByCenterOffset
    :: (Ord b, Ord c, Num c, Show c)
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
  sortOn (abs . subtract midpoint . view (l . f)) placed


correctTitle :: SearchResult Identity -> Text
correctTitle = trimTo 37 "..." . sr_title


trimTo :: Int -> Text -> Text -> Text
trimTo sz rest t
  | T.length t > sz
  = T.take sz t <> rest
  | otherwise = t


ySize :: Float
ySize = view _y $ measureText' (denormalizePt 0) "x"


spaceResult' :: SizedRegionData (SearchResult Identity) -> L.Html ()
spaceResult' Barrier{} = pure ()
spaceResult' (SizedRegionData pt (Region x y _ h) d) = do
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
        , L.width_  "14" -- (T.pack $ show h)
        , L.height_ "14" -- (T.pack $ show h)
        ]
      L.a_ [ L.href_ $ sr_uri d
           , makeAttribute "data-docid" $ T.pack $ show $ unDocId $ sr_id d
           , L.onmouseover_ "tt(this)"
           , L.onmouseout_  "untt(this)"
           ] $ L.toHtml title


data SizedRegionData a
  = SizedRegionData
    { srd_size   :: Float  -- in pts
    , srd_region :: Region
    , srd_data   :: a
    }
  | Barrier
    { srd_region :: Region
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
denormalizePt prept = 10 + prept * 6


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
fitIn sw w x
  | x + w >= sw
  = sw - w
  | otherwise = x



-- determined experimentally
measureText' :: Float -> Text -> V2 Float
measureText' pt t = V2 (8 * fromIntegral (T.length t)) 18 ^* (pt / 10)


-- icons are square and fill the entire lineheight, so extend the width by that
extendForIcon :: V2 Float -> V2 Float
extendForIcon (V2 x y) = V2 (x + y) y

