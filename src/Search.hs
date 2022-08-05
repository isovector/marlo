{-# OPTIONS_GHC -Wno-orphans #-}

module Search where

import           Config
import           Control.Applicative (liftA2)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (evalState, gets, modify)
import           DB
import           Data.Bifunctor (first)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.Functor.Contravariant ((>$<))
import           Data.Functor.Identity (Identity)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe, catMaybes, mapMaybe)
import           Data.Proxy
import           Data.RectPacking
import qualified Data.Set as S
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           GHC.Generics (Generic)
import           Hasql.Connection (acquire, Connection)
import           Hasql.Session (run, statement)
import qualified Lucid as L
import           Network.URI (escapeURIString, isUnescapedInURI)
import           Network.Wai.Application.Static (defaultWebAppSettings, ssMaxAge)
import qualified Network.Wai.Handler.Warp as W
import           Rel8 hiding (max, index)
import           Rel8.TextSearch
import           Search.Parser (searchParser)
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Servant.Server.Generic ()
import           Text.Megaparsec (parse, errorBundlePretty)
import           Types
import           Utils (paginate, timing)
import           WaiAppStatic.Types (MaxAge(NoMaxAge))


data SearchResult f = SearchResult
  { sr_ranking :: !(Column f Float)
  , sr_id      :: !(Column f DocId)
  , sr_uri     :: !(Column f Text)
  , sr_title   :: !(Column f Text)
  , sr_stats   :: !(DiscoveryStats f)
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Eq (SearchResult Identity)

compileSearch :: Search Text -> Query (SearchResult Expr)
compileSearch q = orderBy (sr_ranking >$< desc) $ do
  d <-
    case compile' q of
      Match ts -> matching ts
      Full qu -> qu
  pure $ SearchResult
    { sr_ranking = rank (d_search d) (lit q') rDISTANCE
    , sr_id      = d_docId $ d_table d
    , sr_uri     = d_uri   $ d_table d
    , sr_title   = d_title $ d_table d
    , sr_stats   = d_stats $ d_table d
    }
  where
    q' = compileQuery q


data IL
  = Match Tsquery
  | Full (Query (Discovery' Expr))

compile' :: Search Text -> IL
compile' (Term txt) = Match $ TsqTerm txt
compile' (Phrase []) = Match $ TsqTerm ""
compile' (Phrase txts) = Match $ foldr1 TsqPhrase $ fmap TsqTerm txts
compile' (Negate se) =
  case compile' se of
    Match ts -> Match $ TsqNot ts
    Full qu -> Full $ except (each discoverySchema') qu
compile' (And lhs rhs) = merge TsqAnd intersect (compile' lhs) (compile' rhs)
compile' (Or lhs rhs) = merge TsqOr union (compile' lhs) (compile' rhs)
compile' (SiteLike t) = Full $ do
  d <- each discoverySchema'
  where_ $ like (lit $ "%" <> t <> "%") (d_uri $ d_table d)
       &&. d_state (d_table d) ==. lit Explored
  pure d

merge
    :: (Tsquery -> Tsquery -> Tsquery)
    -> (Query (Discovery' Expr) -> Query (Discovery' Expr) -> Query (Discovery' Expr))
    -> IL
    -> IL
    -> IL
merge t _ (Match t1) (Match t2) = Match $ t t1 t2
merge _ q (Match t1) (Full q2) = Full $ q (matching t1) q2
merge _ q (Full q1) (Match t2) = Full $ q q1 (matching t2)
merge _ q (Full q1) (Full q2) = Full $ q q1 q2

matching :: Tsquery -> Query (Discovery' Expr)
matching q = do
  d <- each discoverySchema'
  where_ $ match (d_search d) (lit q)
       &&. d_state (d_table d) ==. lit Explored
  pure d


compileQuery :: Search Text -> Tsquery
compileQuery (Phrase []) = TsqTerm ""
compileQuery (Phrase wids) =
  foldr1 TsqPhrase $ fmap TsqTerm wids
compileQuery (Term wid) = TsqTerm wid
compileQuery (And lhs rhs) = TsqAnd (compileQuery lhs) (compileQuery rhs)
compileQuery (Or lhs rhs) = TsqOr (compileQuery lhs) (compileQuery rhs)
compileQuery (Negate lhs) = TsqNot (compileQuery lhs)
compileQuery (SiteLike _) = TsqTerm ""


getSnippet :: DocId -> Tsquery -> Query (Expr Text)
getSnippet did q = do
  d <- d_table <$> each discoverySchema'
  where_ $ d_docId d ==. lit did
  pure $ headline (d_content d <>. " " <>. d_headings d <>. " " <>. d_comments d) $ lit q



home :: Connection -> Handler (L.Html ())
home conn = do
  sizes <- liftIO $ do
    Right sizes <- fmap (fmap M.fromList) $
      flip run conn $ statement () $ select $ do
        aggregate $ do
          d <- each discoverySchema
          pure (groupBy $ d_state d, countStar)
    pure sizes
  pure $
    L.html_ $ do
      L.head_ $ do
        L.link_ [L.rel_ "stylesheet", L.href_ "style.css" ]
        L.title_ "marlo: search, for humans"
      L.body_ $ L.div_ $ do
        searchBar Traditional ""
        L.div_ [L.class_ "metrics"] $ do
          L.span_ $ mconcat
            [ "Indexed: "
            , L.toHtml (commafy $ show $ fromMaybe 0 (M.lookup Explored sizes))
            , " / Discovered: "
            , L.toHtml (commafy $ show $ fromMaybe 0 (M.lookup Discovered sizes))
            ]


commafy :: String -> String
commafy
  = T.unpack
  . T.intercalate ","
  . reverse
  . fmap T.reverse
  . T.chunksOf 3
  . T.reverse
  . T.pack


searchBar :: SearchVariety -> Text -> L.Html ()
searchBar v t =
  L.div_ [L.class_ "logo-box"] $ do
    L.form_ [ L.action_ "/search", L.method_ "GET" ] $ do
      L.h1_ "mar"
      L.input_ $
        [ L.id_ "query"
        , L.type_ "text"
        , L.name_ "q"
        , L.value_ t
        ] <>
        [ L.autofocus_
        | t == ""
        ]
      L.select_ [ L.name_ "v" ] $ do
        L.option_ (selected Traditional [ L.value_ "traditional" ]) "traditional"
        L.option_ (selected Spatial     [ L.value_ "spatial" ])     "spatial"
  where
    selected v' z
      | v == v' = L.selected_ "selected" : z
      | otherwise = z

traditionalSearch :: Connection -> Search Text -> Maybe Int -> Handler (L.Html ())
traditionalSearch conn q mpage = do
  let pagenum = Prelude.max 0 $ maybe 0 (subtract 1) mpage
      pagesize :: Num a => a
      pagesize = 20
  (cnt, docs, snips) <- liftIO $ do
    putStrLn $ mappend "trad search: " $ T.unpack $ encodeQuery q
    writeFile "/tmp/lastquery.sql" $ showQuery $ compileSearch q
    Right (cnt, docs) <- timing "find documents" $ fmap (fmap unzip) $
      flip run conn
        $ statement ()
        $ select
        $ paginate pagesize (fromIntegral pagenum)
        $ let x = compileSearch q
           in liftA2 (,) (countRows x) x
    snips <- timing "building snippets" $
      for docs $ \doc -> do
        Right [snip] <- flip run conn
          $ statement ()
          $ select
          $ getSnippet (sr_id doc) $ compileQuery q
        pure snip
    pure (fromMaybe 0 (listToMaybe cnt), docs, snips)
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
          searchBar Traditional $ encodeQuery q
          for_ (zip docs snips) $ uncurry tradResult
          let eq = escape $ encodeQuery q
          when (pagenum > 0) $ do
            L.a_ [L.href_ $ "/search?q=" <> eq <> "&p=" <> T.pack (show pagenum)  ] "Prev"
          when ((pagenum + 1) * pagesize < fromIntegral cnt) $ do
            L.a_ [L.href_ $ "/search?q=" <> eq <> "&p=" <> T.pack (show (pagenum + 2))  ] "Next"
  where
    escape = T.pack . escapeURIString isUnescapedInURI . T.unpack


makeRect :: SearchResult Identity -> Rect (SearchResult Identity)
makeRect sr = Rect
  { r_pos = V2 (log $ max 1 $ fromIntegral $ ds_js $ sr_stats sr) (sr_ranking sr * 100)
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
  let qd = foldr place (makeTree (200, 50) Nothing) $ fmap makeRect docs
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



search :: Connection -> Maybe SearchVariety -> Maybe (Search Text) -> Maybe Int -> Handler (L.Html ())
search _ _ Nothing _ = pure $ "Give me some keywords, punk!"
search conn (fromMaybe Traditional -> Traditional) (Just q) mpage = traditionalSearch conn q mpage
search conn (fromMaybe Traditional -> Spatial) (Just q) _ = spatialSearch conn q


encodeQuery :: Search Text -> Text
encodeQuery (Term kw) = coerce kw
encodeQuery (Phrase keys) = "\"" <> (T.intercalate " " $ coerce keys) <> "\""
encodeQuery (Negate q) = "-(" <> encodeQuery q <> ")"
encodeQuery (And q1 q2) = encodeQuery q1 <> " " <> encodeQuery q2
encodeQuery (Or q1 q2) = "(" <> encodeQuery q1 <> ") OR (" <> encodeQuery q2 <> ")"
encodeQuery (SiteLike t) = "site:" <> t


tradResult :: SearchResult Rel8.Result -> Text -> L.Html ()
tradResult d snip =
  L.div_ [L.class_ "result"] $ do
    L.span_ [L.class_ "url"] $ L.a_ [L.href_ $ sr_uri d] $ L.toHtml $ sr_uri d
    L.span_ [L.class_ "title"] $ L.a_ [L.href_ $ sr_uri d] $ L.toHtml title
    L.p_ [L.class_ "snippet"] $ L.toHtmlRaw snip
  where
    title =
      case T.strip $ sr_title d of
        "" -> "(no title)"
        x -> x

spaceResult :: (SearchResult Rel8.Result, (Int, Int, Int, Int)) -> L.Html ()
spaceResult (d, (x, y, _, _)) =
    L.span_
      [ L.class_ "title"
      , L.style_ $ mconcat
          [ "position: absolute;"
          , "top: "
          , T.pack $ show $ (* 15) y
          , "; "
          , "left: "
          , T.pack $ show $ (* 5) x
          ]
      ] $ L.a_ [L.href_ $ sr_uri d] $ L.toHtml title
  where
    title =
      case T.strip $ sr_title d of
        "" -> "(no title)"
        t -> t

--------------------------------------------------------------------------------

data SearchVariety
  = Traditional
  | Spatial
  deriving (Eq, Ord, Show, Prelude.Enum, Bounded)

type TestApi =
       Get '[HTML] (L.Html ())
  :<|> "search"
        :> QueryParam "v" SearchVariety
        :> QueryParam "q" (Search Text)
        :> QueryParam "p" Int
        :> Get '[HTML] (L.Html ())
  :<|> Raw


instance FromHttpApiData [Text] where
  parseQueryParam = Right . T.split (== ' ')

server :: Connection -> Server TestApi
server conn = home conn
         :<|> search conn
         :<|> serveDirectoryWith (defaultWebAppSettings "static") { ssMaxAge = NoMaxAge }

instance FromHttpApiData (Search Text) where
  parseQueryParam = first (T.pack . errorBundlePretty) . parse searchParser ""

instance FromHttpApiData SearchVariety where
  parseQueryParam "traditional" = Right Traditional
  parseQueryParam "spatial"     = Right Spatial
  parseQueryParam _             = Left "SearchVariety must be one of 'traditional' or 'spatial'"

runTestServer :: Connection -> W.Port -> IO ()
runTestServer conn port = W.run port $ serve (Proxy @TestApi) $ server conn

main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  runTestServer conn cfg_port

