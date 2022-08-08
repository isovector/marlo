{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Search.Spatial () where

import           Control.Arrow ((&&&), second)
import           Control.Exception
import           Control.Lens (view)
import           Control.Monad.State (evalState, gets, modify, when)
import           DB
import           Data.Bool (bool)
import           Data.Foldable (traverse_, asum)
import           Data.Function (fix)
import           Data.List (sortOn)
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe, isJust)
import           Data.Monoid
import           Data.QuadAreaTree
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Debug.Trace (trace, traceShowId)
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
  -- type SearchMethodResult 'Spatial = QuadTree (Maybe (Rect (SearchResult Identity)))
  type SearchMethodResult 'Spatial = QuadTree (Maybe (SearchResult Identity))
  limitStrategy = Limit 100
  accumResults _ _
    = evaluate
    . newLayoutAlgorithm
    . fmap (\sr -> sr { sr_title = correctTitle sr })

  -- accumResults _ _ docs = do
  --   let best = maximum $ fmap sr_ranking docs
  --   let rs = fmap makeRect
  --          $ fmap (\x -> x { sr_ranking = best - sr_ranking x })
  --          $ filter (not . T.null . T.strip . sr_title) docs
  --   evaluate $ foldr place (makeTree (Region 0 0 150 34) Nothing) rs
  showResults
    = traverse_ spaceResult
    . uniqueTiles
    . mapMaybe sequence
    . tile
    -- . fmap (fmap r_data)
  debugResults =
    putStrLn . showTree (maybe ' ' $ const 'X' )
    -- $ toEnum . (+33) . view _x . r_size )


correctTitle :: SearchResult Identity -> Text
correctTitle = trimTo 40 "..." . chopTitle . sr_title

measureText :: Text -> V2 Int
measureText s = V2 (T.length s + 1) 1
-- plus one for the icon


makeRect :: SearchResult Identity -> Rect (SearchResult Identity)
makeRect sr = Rect
  { r_pos = V2 ((* 1.4) $ log $ max 1 $ fromIntegral $ fromMaybe 1e6 $ sr_popularity sr)
               (sr_ranking sr * 5)
  , r_size = measureText title'
  , r_data = sr { sr_title = title' }
  }
  where
    title'
      = trimTo 40 "..."
      $ chopTitle
      $ sr_title sr


trimTo :: Int -> Text -> Text -> Text
trimTo sz rest t
  | T.length t > sz
  = T.take sz t <> rest
  | otherwise = t


chopTitle :: Text -> Text
chopTitle t = fromMaybe t $ asum
  [ onlyIfDifferent (dropTail [ '|', '-', '·', '\8211' ]) t
  , onlyIfDifferent (dropHead [ ':' ]) t
  ]
  where
    dropTail, dropHead :: [Char] -> Text -> Text
    dropTail els
      = T.intercalate ""
      . init
      . T.split (flip elem els)
    dropHead els
      = T.intercalate ""
      . drop 1
      . T.split (flip elem els)


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


spaceResult :: (Region, SearchResult Rel8.Result) -> L.Html ()
spaceResult (Region x y _ _, d) = do
  let title = T.strip $ sr_title d
  when (not $ T.null title) $ do
    L.span_
        [ L.class_ "spatial-title"
        , L.style_ $ mconcat
            [ "position: absolute;"
            , "top: "
            , T.pack $ show $ 250 + y * 18
            , "; "
            , "left: "
            , T.pack $ show $ 50 + x * 11
            ]
        ] $ do
      let uri = unsafeURI $ T.unpack $ sr_uri d
      L.img_
        [ L.src_ $ T.pack $ show $ uri { uriPath = "/favicon.ico" }
        , L.width_  "12"
        , L.height_ "12"
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


place :: Eq a => Rect a -> QuadTree (Maybe (Rect a)) -> QuadTree (Maybe (Rect a))
place r0 q0
  | not $ inBounds q0 $ rectToRegion r0 = q0
  | otherwise = go (0 :: Int) r0 q0
  where
    go 15 _ qt = qt
    go n r qt =
      case getFirst $ hitTest First (rectToRegion r) qt of
        Nothing -> fillRect r qt
        Just re ->
          go (n + 1) (offsetBy r re) qt


rectToRegion :: Rect a -> Region
rectToRegion Rect{r_pos = fmap round -> V2 x y, r_size = V2 w h } = Region x y w h


fillRect :: Rect a -> QuadTree (Maybe (Rect a)) -> QuadTree (Maybe (Rect a))
fillRect r = fill (Just r) $ rectToRegion r


------------------------------------------------------------------------------
-- | Get the length of a vector from the center point  to the end of the size
-- envelope
extent :: V2 Float -> V2 Int -> Float
extent dir (fmap fromIntegral -> V2 w h) =
  norm $ V2 (dot dir $ V2 (w / 2) 0) (dot dir $ V2 0 (h / 2))


offsetBy :: Rect a -> Rect a -> Rect a
offsetBy want collide =
  let dir = unzero $ normalize $ rectCenter want - rectCenter collide
      get_ext = extent dir . r_size
      new_center = rectCenter want + dir ^* ((get_ext want + get_ext collide))
      res = want { r_pos = uncenter (r_size want) new_center }
   in -- trace (show ("offsetting ", r_pos want, " to ", r_pos res, dir ))
      res

unzero :: V2 Float -> V2 Float
unzero v | quadrance v < 0.1  = V2 0.707 0.707
unzero v = v

------------------------------------------------------------------------------


newLayoutAlgorithm
    :: [SearchResult Identity]
    -> QuadTree (Maybe (SearchResult Identity))
newLayoutAlgorithm res = do
  let n        = length res
      placed0  = fmap (id &&& plop) res
      midpoint = sum (fmap snd placed0) / fromIntegral n
      ordered  = sortOn (quadrance . snd) $ fmap (second $ subtract midpoint) placed0
      sz       = (^) @Int @Int 2 14
      tree0    = makeTree (Region (-sz) (-sz) (sz * 2) (sz * 2)) Nothing
      tree     = foldr (uncurry place') tree0 ordered
  renormalize (\(Region _ _ w h) -> Region 0 0 w h)
    $ tighten isJust
    $ fmap (fmap snd)
    $ tree


place'
    :: SearchResult Identity
    -> V2 Float
    -> QuadTree (Maybe ((V2 Float, Region), SearchResult Identity))
    -> QuadTree (Maybe ((V2 Float, Region), SearchResult Identity))
place' sr p0 q = do
  let V2 w h = measureText $ sr_title sr
  flip fix p0 $ \go p@(V2 x y) -> do
    let r = Region (round x) (round y) w h
    case inBounds q r of
      False -> q
      True ->
        case getFirst $ hitTest First r q of
          Just (hit, _) -> go $ r_pos $ offsetBy (regionToRect p r) $ uncurry regionToRect hit
          Nothing -> fill (Just ((p, r), sr)) r q


regionToRect :: V2 Float -> Region -> Rect ()
regionToRect v2 (Region _ _ w h) = Rect
    { r_pos = v2
    , r_size = V2 w h
    , r_data = ()
    }



plop :: SearchResult Identity -> V2 Float
plop = r_pos . makeRect

