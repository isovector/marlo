{-# LANGUAGE OverloadedStrings #-}

module Search.Parser where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Types
import qualified Data.Text as T
import Keywords (isKeywordLetter)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Char (string, char, space1)
import Data.Foldable (asum)
import Text.Megaparsec.Char.Lexer (symbol, lexeme)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAlphaNum)

type Parser = Parsec Void Text

term :: Parser (Search Text)
term = asum
  [ between (lexeme sp $ char '(') (lexeme sp $ char ')') expr
  , siteParser
  , phraseParser
  , fmap Term keywordParser
  ]

sp :: Parser ()
sp = L.space space1 empty empty

expr :: Parser (Search Text)
expr = makeExprParser term
  [ [ Prefix $ Negate <$ symbol sp "-"   ]
  , [ InfixL $ And    <$ symbol sp ""    ]
  , [ InfixL $ And    <$ symbol sp "AND" ]
  , [ InfixL $ Or     <$ symbol sp "OR"  ]
  ]

searchParser :: Parser (Search Text)
searchParser = expr <* eof

phraseParser :: Parser (Search Text)
phraseParser = do
  _ <- symbol sp "\""
  x <- many keywordParser
  _ <- symbol sp "\""
  pure $ Phrase x

keywordParser :: Parser Text
keywordParser = try $ do
  k <- lexeme sp $ some $ satisfy isAlphaNum
  case k of
    "AND" -> empty
    "OR" -> empty
    _ -> pure ()
  pure $ T.pack k

siteParser :: Parser (Search a)
siteParser = lexeme sp $ do
  _ <- string "site:"
  fmap (SiteLike . T.pack) $ many $ satisfy $ \c -> or
    [ isKeywordLetter c
    , c == '/'
    , c == '.'
    , c == ':'
    ]

