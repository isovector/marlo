{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Lasercutter.HTML
  ( module Lasercutter.HTML
  , (/\)
  , (\/)
  , module Algebra.Heyting
  ) where

import           Algebra.Heyting
import           Algebra.Lattice ((/\), (\/))
import           Data.Maybe (maybeToList, listToMaybe, isJust)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.OverloadedLabels
import           GHC.TypeLits (symbolVal, KnownSymbol)
import           Lasercutter
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree

type Html = TagTree Text
type HtmlParser = Parser (Set Text) (TagTree Text)


instance KnownSymbol id => IsLabel id (Html -> Bool) where
  fromLabel = hasId $ T.pack $ symbolVal $ Proxy @id

instance IsString (Html -> Bool) where
  fromString [] = pure False
  fromString ('.' : s) = hasClass $ T.pack s
  fromString ('#' : s) = hasId    $ T.pack s
  fromString s         = hasTag   $ T.pack s


instance IsTree (TagTree Text) where
  getChildren (TagBranch _ _ tts) = tts
  getChildren (TagLeaf _) = []



summarize :: TagTree Text -> Set Text
summarize = S.fromList . maybeToList . getTag


hasTag :: Text -> Html -> Bool
hasTag t = maybe False (== t) . getTag


getTag :: TagTree Text -> Maybe Text
getTag (TagBranch txt _ _)       = Just txt
getTag (TagLeaf (TagOpen txt _)) = Just txt
getTag _                         = Nothing


------------------------------------------------------------------------------
-- | Specialized 'target' so we can use the overloaded strings and labels.
match :: (Html -> Bool) -> HtmlParser a -> HtmlParser [a]
match = target


isContentNode :: HtmlParser Bool
isContentNode
  = onBreadcrumbs
  $ S.null . S.intersection nonContentNodes


targetOne :: (t -> Bool) -> Parser bc t a -> Parser bc t a
targetOne f = expect . fmap listToMaybe . target f


text :: HtmlParser Text
text = expect $ fmap listToMaybe $ texts


texts :: HtmlParser [Text]
texts = targetMap getText


contentTexts :: HtmlParser [Text]
contentTexts
  = fmap catMaybes
  $ target isText
  $ ifS isContentNode (proj getText) (pure Nothing)


isText :: Html -> Bool
isText = isJust . getText


hasId :: Text -> Html -> Bool
hasId t = maybe False (== t) . getAttr "id"


getText :: Html -> Maybe Text
getText (TagLeaf (TagText txt)) = Just txt
getText _ = Nothing


getAttr :: Text -> Html -> Maybe Text
getAttr a (TagBranch _ as _)       = lookup a as
getAttr a (TagLeaf (TagOpen _ as)) = lookup a as
getAttr _ (TagLeaf _)              = Nothing


getClasses :: Html -> Set Text
getClasses
  = maybe mempty (S.fromList . T.split (== ' '))
  . getAttr "class"


hasClass :: Text -> Html -> Bool
hasClass t = S.member t . getClasses


attr :: Text -> HtmlParser (Maybe Text)
attr = proj . getAttr


nonContentNodes :: Set Text
nonContentNodes = S.fromList
  [ "style"
  , "script"
  , "noscript"
  , "li"
  , "ul"
  , "ol"
  , "iframe"
  , "nav"
  , "object"
  , "source"
  , "svg"
  , "template"
  , "track"
  , "select"
  , "option"
  , "button"
  , "canvas"
  , "nav"
  , "h1"
  , "h2"
  , "h3"
  , "h4"
  , "h5"
  , "h6"
  , "sup"
  , "sub"
  ]


html :: HtmlParser Text
html = proj $ renderTree . pure

innerHtml :: HtmlParser Text
innerHtml = fmap renderTree $ onChildren self


example :: TagTree Text
example =
  TagBranch "html" [("lang", "en")]
    [ TagBranch "head" []
      [ TagBranch "title" [] [ TagLeaf $ TagText "Hello World!" ]
      , TagBranch "style" [("type", "text/css")]
          [ TagLeaf $ TagText "css"
          ]
      ]
    , TagBranch "body" []
      [ TagBranch "h1" [] [ TagLeaf $ TagText "Hi" ]
      , TagBranch "p" [("id", "lorem")] [ TagLeaf $ TagText "lorem ipsum" ]
      , TagBranch "div" [("class", "main content")]
        [ TagBranch "p" []
          [ TagLeaf $ TagText "more p"
          , TagBranch "b" []
            [ TagLeaf $ TagText "bold"
            ]
          , TagLeaf $ TagText "done"
          ]
        , TagBranch "script" []
          [ TagLeaf $ TagText "dont want no scripts"
          ]
        ]
      ]
    ]


main :: IO ()
main = print $ runParser summarize example $ sequenceA
  [ match (".main" \/ #lorem) innerHtml
  ]

