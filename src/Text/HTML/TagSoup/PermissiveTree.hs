{-# OPTIONS_GHC -Wno-orphans #-}

module Text.HTML.TagSoup.PermissiveTree
  ( parsePermissiveTree
  ) where

import           Control.Monad (when, void)
import           Control.Monad.State (State, evalState, modify, gets)
import           Data.Foldable (asum, toList)
import           Data.List (findIndices)
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.Megaparsec hiding (State)


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


instance VisualStream [Tag Text] where
  showTokens _ = T.unpack . renderTags . toList

instance TraversableStream [Tag Text] where
  reachOffsetNoLine _ = id

parsePermissiveTree :: Text -> Maybe (TagTree Text)
parsePermissiveTree
  = either
      (const Nothing)
      (Just . TagBranch "" [])
  . flip evalState []
  . runParserT tree ""
  . filter (not . isIrrelevant)
  . canonicalizeTags
  . parseTags


isIrrelevant :: Tag Text -> Bool
isIrrelevant (TagComment _) = True
isIrrelevant (TagWarning _) = True
isIrrelevant (TagPosition _ _) = True
isIrrelevant _ = False

