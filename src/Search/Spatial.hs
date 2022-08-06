module Search.Spatial (spatialSearch) where

import           Control.Applicative (liftA2)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (evalState, gets, modify)
import           DB
import           Data.Foldable (for_)
import           Data.Functor.Identity (Identity)
import           Data.Maybe (fromMaybe, listToMaybe, catMaybes, mapMaybe)
import           Data.RectPacking
import qualified Data.Set as S
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Hasql.Connection (Connection)
import           Hasql.Session (run, statement)
import qualified Lucid as L
import           Rel8 hiding (max, index)
import           Search.Compiler
import           Search.SearchBar (searchBar)
import           Servant
import           Servant.Server.Generic ()
import           Types
import           Utils (timing)



makeRect :: SearchResult Identity -> Rect (SearchResult Identity)
makeRect sr = Rect
  { r_pos = V2 ((+ 30) $ log $ max 1 $ fromIntegral $ ds_js $ sr_stats sr)
               (sr_ranking sr * 100)
  , r_size = measureText $ sr_title sr
  , r_data = sr
  }


spatialSearch :: Connection -> Search Text -> Handler (L.Html ())
spatialSearch conn q = do
  (cnt, docs) <- liftIO $ do
    putStrLn $ mappend "spatial search: " $ T.unpack $ encodeQuery q
    writeFile "/tmp/lastquery.sql" $ showQuery $ compileSearch q
    Right (cnt, docs) <- timing "find documents" $ fmap (fmap unzip) $
      flip run conn
        $ statement ()
        $ select
        $ limit 1000
        $ let x = compileSearch q
           in liftA2 (,) (countRows x) x
    pure (fromMaybe 0 (listToMaybe cnt), docs)
  let qd = foldr place (makeTree (250, 80) Nothing) $ fmap makeRect docs
  pure $
    L.html_ $ do
      L.head_ $ do
        L.title_ $ mconcat
          [ "marlo search - results for "
          , L.toHtml (encodeQuery q)
          , " (" <> fromString (show cnt)
          , ")"
          ]
        L.link_ [L.rel_ "stylesheet", L.href_ "results.css" ]
      L.body_ $ do
        L.div_ [L.class_ "box"] $ do
          searchBar Spatial $ encodeQuery q
          for_ (uniqueTiles $ mapMaybe sequenceTile $ tile $ tmap (fmap r_data) qd) spaceResult


uniqueTiles :: [Tile (SearchResult Identity)] -> [Tile (SearchResult Identity)]
uniqueTiles ts = flip evalState mempty $ fmap catMaybes $
  for ts $ \t@(sr, _) -> do
    gets (S.member $ sr_id sr) >>= \case
       False -> do
         modify $ S.insert $ sr_id sr
         pure $ Just t
       True -> pure Nothing

spaceResult :: (SearchResult Rel8.Result, (Int, Int, Int, Int)) -> L.Html ()
spaceResult (d, (x, y, _, _)) =
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

