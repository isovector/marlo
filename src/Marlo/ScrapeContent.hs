{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Marlo.ScrapeContent where

import           DB.PageContent
import           Data.Functor.Identity (Identity)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Lasercutter
import           Lasercutter.HTML



mainContent :: HtmlParser Text
mainContent
  = asum
  $ fmap (\sel -> targetOne sel $ failIfEmpty contentText)
    [
      "div" /\ ".entry-content"
    , "div" /\ ".content"
    , "div" /\ ".pjgm-postcontent"
    , "div" /\ ".PostsPage-postContent"
    , "div" /\ ".ArticleBody-articleBody"
    , "div" /\ "#mw-content-text"
    , "article"
    , "main"
    , "div" /\ #content
    , "td" /\ ".mainsection"
    , "body"
    ]


headingsContent :: HtmlParser Text
headingsContent
  = fmap (T.intercalate " ")
  $ fmap (filter $ not . T.null)
  $ liftA2 (<>)
      <$> match "h1" text
      <*> match "h2" text


commentsContent :: HtmlParser Text
commentsContent
  = fmap (fromMaybe "")
  $ try
  $ asum
  $ fmap (\sel -> targetOne sel $ failIfEmpty contentText)
    [ "div" /\ ".comments-area"
    , "div" /\ ".comments-page"
    , "div" /\ ".comments"
    , "div" /\ ".PostsPage-commentsSection"
    , "div" /\ "#comments"
    ]

rankContent :: HtmlParser (PageContent Identity)
rankContent = PageContent
  <$> headingsContent
  <*> mainContent
  <*> commentsContent

