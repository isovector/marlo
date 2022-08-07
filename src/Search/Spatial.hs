{-# OPTIONS_GHC -Wno-orphans #-}

module Search.Spatial () where

import           Control.Exception
import           Control.Monad.State (evalState, gets, modify)
import           DB
import           Data.Foldable (traverse_)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.RectPacking
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Lucid as L
import           Rel8 hiding (evaluate, max, index)
import           Search.Machinery
import           Servant.Server.Generic ()
import           Types


-- there is ANOTHER bug in the quadtree
-- this time maybe when the starting xy are negative?

instance SearchMethod 'Spatial where
  type SearchMethodResult 'Spatial = QuadTree (Maybe (Rect (SearchResult Identity)))
  limitStrategy = Limit 500
  accumResults _ _ docs = do
    let rs = fmap makeRect docs
    evaluate $ foldr place (makeTree (Region 0 0 150 80) Nothing) rs
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
chopTitle
  = T.intercalate ""
  . init
  . T.split
      (flip elem
        [ '|'
        , '-'
        , ':'
        , '·'
        , '\8211'
        ]
      )


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
      [ L.class_ "spatial-title"
      , L.style_ $ mconcat
          [ "position: absolute;"
          , "top: "
          , T.pack $ show $ 250 + y * 18
          , "; "
          , "left: "
          , T.pack $ show $ 50 + x * 11
          ]
      ] $ L.a_ [L.href_ $ sr_uri d] $ L.toHtml title
  where
    title =
      case T.strip $ sr_title d of
        "" -> "(no title)"
        t -> t

