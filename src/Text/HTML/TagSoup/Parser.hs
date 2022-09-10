module Text.HTML.TagSoup.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void (Void)
import Control.Monad (void)
import qualified Data.Text as T
import Data.Char (isAlpha)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup (Tag(..))

type Parser = Parsec Void Text

sp :: Parser ()
sp = L.space space1 empty $ L.skipBlockComment "<!--" "-->"


l :: Parser a -> Parser a
l = L.lexeme sp

tag :: Parser ()
tag = void $ string "<"

tagEnd :: Parser ()
tagEnd = void $ l $ string ">"

tagEndClose :: Parser ()
tagEndClose = void $ l $ string "/>"

tagShut :: Parser ()
tagShut = void $ string "</"

attr :: Parser (Text, Text)
attr = do
  name <- attrName
  val <- optional $ do
    void $ l "="
    str
  pure (name, fromMaybe name val)

elName :: Parser Text
elName = l strChunk

openElement :: Parser [Tag Text]
openElement = do
  tag
  el <- elName
  attrs <- many attr
  (:)
    <$> pure (TagOpen el attrs)
    <*> asum
      [ [] <$ tagEnd
      , [TagClose el] <$ tagEndClose
      ]


closeElement :: Parser (Tag Text)
closeElement = do
  tagShut
  el <- elName
  -- chop out rest
  pure $ TagClose el



main :: IO ()
main = print $ parse openElement "" "<test yo blah='ok soup'/>"

attrName :: Parser Text
attrName = l $ fmap T.pack $ some $ satisfy $ \x -> isAlpha x || x == '-'

attrValue :: Parser Text
attrValue = str

str :: Parser Text
str = l $ asum
  [ between "\"" "\"" strChunks
  , between "\'" "\'" strChunks
  , strChunk
  ]

strChunks :: Parser Text
strChunks
  = fmap (T.intercalate " ")
  $ sepBy strChunk
  $ asum [ " ", "\r", "\n" ]

strChunk :: Parser Text
strChunk = fmap T.pack $ many $ satisfy $ \x -> not $ any (== x)
  [ '<'
  , '>'
  , '/'
  , ' '
  , '\n'
  , '\r'
  , '"'
  , '\''
  ]

