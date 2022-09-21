module ReverseSearch (reverseSearch) where

import qualified Algebra.Graph as G
import           Algebra.Graph.AdjacencyMap.Algorithm (bfsForest)
import           Algebra.Graph.Class
import           Control.Monad.State
import           DB hiding (connect)
import           Data.Foldable (fold)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Data.Tree (Tree (Node))
import qualified Lucid as L
import           Rel8
import           Rel8.Arrays
import           Search.Common
import           Servant (Handler)
import           Types
import Data.Int (Int16)


reverseSearch :: Connection -> [Text] -> Handler (L.Html ())
reverseSearch conn (T.intercalate "/" -> uri) = do
  liftIO $ putStrLn $ "rev search: " <> T.unpack uri
  d <- liftIO $ do
    Right [d] <- doSelect conn $ do
      d <- each documentSchema
      where_ $ d_uri d ==. lit uri
      pure d
    pure d

  gr <- liftIO
      $ flip evalStateT mempty
      $ graphSearch conn d
      $ \d' -> reverseDeps ((==. lit (d_docId d')) . d_docId)

  pure $
    L.html_ $ do
      L.head_ $ do
        L.title_ $ mconcat
          [ "marlo search - reverse search for "
          , L.toHtml uri
          ]
        L.link_ [L.rel_ "stylesheet", L.href_ "/common.css" ]
        L.link_ [L.rel_ "stylesheet", L.href_ "/results.css" ]
        L.script_ [L.type_ "text/javascript", L.src_ "size.js"] $ id @Text ""
      L.body_ $ do
        L.div_ [L.class_ "box"] $ do
          searchBar Spatial defaultSearchDims Nothing
          graphToHtml [d_uri d] $ fmap d_uri gr


isAncestor :: Expr (Distance Int16) -> Expr (Distance Int16) -> Expr Bool
isAncestor a b = arrayAllTrue $ arrayZipWithLt (viewAs a) (viewAs b)


reverseDeps :: (Document Expr -> Expr Bool) -> Query (Document Expr)
reverseDeps which = do
  dst <- each documentSchema
  where_ $ which dst
  e <- each edgesSchema
  dstdisc <- each discoverySchema
  where_ $ e_dst e ==. disc_id dstdisc
  src <- each documentSchema
  where_ $ e_src e ==. d_docId src
  where_ $ isAncestor (d_distance src) (d_distance dst)
  pure src


graphSearch
    :: Connection
    -> Document Identity
       -- ^ starting from
    -> (Document Identity -> Query (Document Expr))
       -- ^ neighbors
    -> StateT (Set DocId) IO (G.Graph (Document Identity))
graphSearch conn d get_neighbors = do
  gets (S.member $ d_docId d) >>= \case
    False -> do
      modify $ S.insert $ d_docId d
      Right ns <- liftIO $ doSelect conn $ get_neighbors d
      gs <- for ns $ \n -> graphSearch conn n get_neighbors
      pure $ foldMap (edge d) ns <> fold gs
    True -> pure empty


fromGraph :: (Graph g, Vertex g ~ a) => G.Graph a -> g
fromGraph = G.foldg empty vertex overlay connect


graphToHtml :: (Ord a, L.ToHtml a) => [a] -> G.Graph a -> L.Html ()
graphToHtml roots = forestToHtml . flip bfsForest roots . fromGraph


forestToHtml :: L.ToHtml a => [Tree a] -> L.Html ()
forestToHtml = (L.ul_ [] .) $ foldMap $ \(Node a trs) -> do
  L.li_ $ do
    L.toHtml a
    forestToHtml trs

