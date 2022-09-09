{-# OPTIONS_GHC -Wno-orphans #-}

module Text.HTML.TagSoup.PermissiveTree where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S
import Data.Set (Set)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike)
import Text.Megaparsec hiding (State)
import Data.Text (Text)
import Data.Void (Void)
import Data.Tree
import qualified Data.Text as T
import Data.Maybe
import Data.Foldable (asum, toList, find)
import Control.Monad.State (State, evalState, runState, modify, gets, StateT, evalStateT)
import Control.Monad (when, void)
import Data.List (findIndices)
import Debug.Trace (traceM)

type Html = TagTree Text

voidTags :: Set Text
voidTags = S.fromList
  [ "area"
  , "base"
  , "br"
  , "col"
  , "command"
  , "embed"
  , "hr"
  , "img"
  , "input"
  , "keygen"
  , "link"
  , "meta"
  , "param"
  , "source"
  , "track"
  , "wbr"
  ]

type TreeParser = ParsecT Void [Tag Text] (State [Text])


openTag :: (Text -> Bool) -> TreeParser (Tag Text)
openTag f = label "open tag" $ flip token mempty $ \case
   t@(TagOpen txt _) | f txt -> pure t
   _ -> Nothing

closeTag :: (Text -> Bool) -> TreeParser (Tag Text)
closeTag f = label "close tag" $ flip token mempty $ \case
   t@(TagClose txt) | f txt -> pure t
   _ -> Nothing

doctype :: TreeParser (TagTree Text)
doctype = do
  fmap TagLeaf $ label "doctype" $ openTag $ (== "!") . T.take 1


disallowNested :: Set Text -> Text -> TreeParser ()
disallowNested ts t = do
  case S.member t ts of
    False -> pure ()
    True -> gets (listToMaybe . take 1) >>= \case
      Nothing -> pure ()
      Just t' ->
        when (t == t') $ do
         modify $ drop 1
         label "nested fallout" empty


unnestableTags :: Set Text
unnestableTags = S.fromList
  [ "p"
  , "li"
  ]


openClose :: TreeParser (TagTree Text)
openClose = do
  to@(TagOpen t attrs) <- openTag $ const True
  disallowNested unnestableTags t

  case S.member t voidTags of
    True -> do
      _ <- optional (closeTag $ (== t))
      pure $ TagLeaf to
    False -> do
      modify (t :)
      -- traceM $ "(((recursing " <> show t
      r <- TagBranch t attrs <$> tree
      -- traceM $ ")))returning " <> show t
      -- gets show >>= traceM
      toclose <- gets head
      when (toclose == t) $ do
        modify $ drop 1
        void $ label ("closing for " <> show t) (closeTag $ (== t))
      pure r


textNode :: TreeParser (Tag Text)
textNode =
  flip token mempty $ \case
     t@(TagText _) -> pure t
     _ -> Nothing


tree :: TreeParser [TagTree Text]
tree = fmap catMaybes $ do
  here <- gets $ take 1
  many $ do
    now <- gets $ take 1
    case here == now of
      False -> empty
      True ->
        asum
          [ fmap Just $ doctype
          , fmap Just $ TagLeaf <$> textNode
          , fmap Just $ try $ openClose
          , (Nothing <$) $ try $ do
              TagClose t <- closeTag $ const True
              gets (findIndices (== t)) >>= \case
                [] -> do
                  -- traceM $ "closing unopened " <> show t
                  pure ()
                (0:_) -> label ("closing something already opened " <> show t) empty
                (n:_) -> do
                  -- gets show >>= traceM
                  -- traceM $ "found an errant /" <> show t <> " at " <> show n
                  modify $ drop $ n
                  -- gets show >>= traceM
                  empty
          ]




foldHtml :: Html -> Maybe (Tree Text)
foldHtml (TagBranch txt _ tts) = Just $ Node txt $ mapMaybe foldHtml tts
foldHtml (TagLeaf (TagOpen txt _)) = Just $ Node txt []
foldHtml _ = Nothing

debugTree :: Html -> String
debugTree = fromMaybe "" . fmap drawTree . fmap (fmap T.unpack) . foldHtml

debugForest :: [Html] -> String
debugForest = drawForest . fmap (fmap T.unpack) . catMaybes . fmap foldHtml

instance VisualStream [Tag Text] where
  showTokens _ = T.unpack . renderTags . toList

instance TraversableStream [Tag Text] where
  reachOffsetNoLine _ = id

parsePermissiveTree :: Text -> Maybe [TagTree Text]
parsePermissiveTree
  = either (const Nothing) Just
  . flip evalState []
  . runParserT tree ""
  . filter (not . isIrrelevant)
  . canonicalizeTags
  . parseTags

main :: IO ()
main = do
  -- print
  putStrLn
    $ either errorBundlePretty id
    $ fmap debugForest
    $ flip evalState []
    $ runParserT tree ""
    $ filter (not . isIrrelevant)
    $ canonicalizeTags
    $ parseTags @Text
    $ "<!DOCTYPE html><HTML dir=\"ltr\" lang=\"en\"><head> <meta charset=\"UTF-8\" /> <meta name=\"robots\" content=\"noodp\" /> <link rel=\"pingback\" href=\"https://automattic.com/xmlrpc.php\" /> <meta name=\"google-site-verification\" content=\"LsUOC70Kd1OVRa84YXSdDMeWfa_tu9k9Cyi9HUFlR60\" /> <link rel=\"start\" href=\"https://automattic.com/\" title=\"Automattic\" /> <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" /> <link rel=\"preconnect\" href=\"https://s0.wp.com\" > <title>Copyright Policy &#8211; Automattic</title> <meta name='robots' content='max-image-preview:large' /> <meta name=\"google-site-verification\" content=\"XECAgs1qhOf9tfH5snpZFPxF4nDR6Jmy58IraJfFVo0\"> <!-- Async WordPress.com Remote Login --> <script id=\"wpcom_remote_login_js\"> </script> </head><body><p><b>yo</i></b><p><strong>hi</strong></p></body></html>"

isIrrelevant :: Tag Text -> Bool
isIrrelevant (TagComment _) = True
isIrrelevant (TagWarning _) = True
isIrrelevant (TagPosition _ _) = True
isIrrelevant _ = False

-- openClose :: StringLike str => str -> [TagTree str]
-- openClose = tagPermissiveTree . parseTags


-- tagPermissiveTree :: Eq str => [Tag str] -> [TagTree str]
-- tagPermissiveTree = g
--     where
--         g :: Eq str => [Tag str] -> [TagTree str]
--         g [] = []
--         g xs = a ++ map TagLeaf (take 1 b) ++ g (drop 1 b)
--             where (a,b) = f xs

--         -- the second tuple is either null or starts with a close
--         f :: Eq str => [Tag str] -> ([TagTree str],[Tag str])
--         f (TagOpen name atts:rest) =
--             case f rest of
--                 (inner,[]) -> (TagLeaf (TagOpen name atts):inner, [])
--                 (inner,TagClose x:xs)
--                     | x == name -> let (a,b) = f xs in (TagBranch name atts inner:a, b)
--                     | otherwise -> let (a,b) = f xs in (TagBranch name atts inner:TagLeaf (TagOpen x []):a, b)
--                 _ -> error "TagSoup.Tree.tagTree: safe as - forall x . isTagClose (snd (f x))"

--         f (TagClose x:xs) = ([], TagClose x:xs)
--         f (x:xs) = (TagLeaf x:a,b)
--             where (a,b) = f xs
--         f [] = ([], [])

