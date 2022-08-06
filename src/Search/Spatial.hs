{-# OPTIONS_GHC -Wno-orphans #-}

module Search.Spatial () where

import           Control.Monad.State (evalState, gets, modify)
import           DB
import           Data.Foldable (for_)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.RectPacking
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Lucid as L
import           Rel8 hiding (max, index)
import           Search.Machinery
import           Servant.Server.Generic ()
import           Types


instance SearchMethod 'Spatial where
  type SearchMethodResult 'Spatial = SearchResult Identity
  limitStrategy = Limit 20
  accumResults _ _ = pure
  showResults docs = do
    let qd = foldr place (makeTree (Region 0 0 250 80) Nothing) $ fmap makeRect docs
    for_ (uniqueTiles $ mapMaybe sequence $ tile $ fmap (fmap r_data) qd) spaceResult


makeRect :: SearchResult Identity -> Rect (SearchResult Identity)
makeRect sr = Rect
  { r_pos = V2 ((+ 30) $ log $ max 1 $ fromIntegral $ ps_js $ sr_stats sr)
               (sr_ranking sr * 100)
  , r_size = measureText $ sr_title sr
  , r_data = sr
  }


uniqueTiles :: [(Region, SearchResult Identity)] -> [(Region, SearchResult Identity)]
uniqueTiles ts = flip evalState mempty $ fmap catMaybes $
  for ts $ \t@(_, sr) -> do
    gets (S.member $ sr_id sr) >>= \case
       False -> do
         modify $ S.insert $ sr_id sr
         pure $ Just t
       True -> pure Nothing


spaceResult :: (Region, SearchResult Rel8.Result) -> L.Html ()
spaceResult (Region x y _ _, d) =
    L.span_
      [ L.class_ "title"
      , L.style_ $ mconcat
          [ "position: absolute;"
          , "top: "
          , T.pack $ show $ 200 + y * 15
          , "; "
          , "left: "
          , T.pack $ show $ 50 + x * 10
          ]
      ] $ L.a_ [L.href_ $ sr_uri d] $ L.toHtml title
  where
    title =
      case T.strip $ sr_title d of
        "" -> "(no title)"
        t -> t

