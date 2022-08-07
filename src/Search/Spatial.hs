{-# OPTIONS_GHC -Wno-orphans #-}

module Search.Spatial () where

import           Control.Exception
import           Control.Monad.State (evalState, gets, modify, when)
import           DB
import           Data.Bool (bool)
import           Data.Foldable (traverse_, asum)
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import           Data.RectPacking
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Lucid as L
import           Network.URI (uriPath)
import           Rel8 hiding (bool, evaluate, max, index)
import           Search.Machinery
import           Servant.Server.Generic ()
import           Types
import           Utils (unsafeURI)


-- there is ANOTHER bug in the quadtree
-- this time maybe when the starting xy are negative?

instance SearchMethod 'Spatial where
  type SearchMethodResult 'Spatial = QuadTree (Maybe (Rect (SearchResult Identity)))
  limitStrategy = Limit 500
  accumResults _ _ docs = do
    let best = maximum $ fmap sr_ranking docs
    let rs = fmap makeRect
           $ fmap (\x -> x { sr_ranking = best - sr_ranking x }) docs
    evaluate $ foldr place (makeTree (Region 0 0 150 200) Nothing) rs
  showResults
    = traverse_ spaceResult
    . uniqueTiles
    . mapMaybe sequence
    . tile
    . fmap (fmap r_data)


makeRect :: SearchResult Identity -> Rect (SearchResult Identity)
makeRect sr = Rect
  { r_pos = V2 (log $ max 1 $ fromIntegral $ ps_js (sr_stats sr) + ps_css (sr_stats sr))
               (sr_ranking sr * 3)
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

