{-# OPTIONS_GHC -Wno-orphans #-}

module Search.Parser where

import           Control.Monad.Combinators.Expr
import           Data.Char (isAlphaNum)
import           Data.Foldable (asum)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           Keywords (isKeywordLetter)
import           Servant (ToHttpApiData, toQueryParam)
import           Text.Megaparsec
import           Text.Megaparsec.Char (string, char, space1)
import           Text.Megaparsec.Char.Lexer (symbol, lexeme, decimal)
import qualified Text.Megaparsec.Char.Lexer as L
import           Types

------------------------------------------------------------------------------
-- Not actually part of the parser, but needs to evolve together
instance ToHttpApiData (Search Text) where
  toQueryParam = encodeQuery

encodeQuery :: Search Text -> Text
encodeQuery (Term kw) = kw
encodeQuery (Phrase keys) = "\"" <> (T.intercalate " " $ keys) <> "\""
encodeQuery (Negate q) = "-(" <> encodeQuery q <> ")"
encodeQuery (And q1 q2) = encodeQuery q1 <> " " <> encodeQuery q2
encodeQuery (Or q1 q2) = "(" <> encodeQuery q1 <> ") OR (" <> encodeQuery q2 <> ")"
encodeQuery (SiteLike t) = "site:" <> t
encodeQuery (WithProperty prop op) = "where:" <> encodeProp prop <> encodeOp op


encodeProp :: SiteProp -> Text
encodeProp JSBundle = "js"
encodeProp CSSBundle = "css"


encodeOp :: Predicate -> Text
encodeOp (Exactly n) = "=" <> T.pack (show n)
encodeOp (LessThan n) = "<" <> T.pack (show n)
encodeOp (GreaterThan n) = ">" <> T.pack (show n)


------------------------------------------------------------------------------


type Parser = Parsec Void Text

term :: Parser (Search Text)
term = asum
  [ between (lexeme sp $ char '(') (lexeme sp $ char ')') expr
  , siteParser
  , phraseParser
  , withPropParser
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

withPropParser :: Parser (Search Text)
withPropParser = do
  prop <- propParser
  op <- asum
    [ LessThan <$ symbol sp "<"
    , GreaterThan <$ symbol sp ">"
    , Exactly <$ symbol sp "=="
    , Exactly <$ symbol sp "="
    ]
  val <- decimal
  pure $ WithProperty prop $ op val

propParser :: Parser SiteProp
propParser = do
  _ <- string "with:"
  asum
    [ JSBundle <$ symbol sp "js"
    , CSSBundle <$ symbol sp "javascript"
    , CSSBundle <$ symbol sp "css"
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

