{-# OPTIONS_GHC -Wno-orphans #-}
module Search.Compiler
  ( compileSearch
  , compileQuery
  ) where

import DB
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Prelude hiding (null)
import Rel8 hiding (max, index)
import Rel8.TextSearch
import Servant.Server.Generic ()
import Types


compileSearch :: Search Text -> Query (SearchResult Expr)
compileSearch q = orderBy ((sr_ranking >$< asc) <> (sr_id >$< asc)) $ do
  d <- do
    d <- case compile' q of
      Match ts -> matching ts
      Full qu -> qu

    where_ $ d_flags (d_table d) ==. lit mempty
    pure d
  popularity <- optional $ do
      dom <- each domainsSchema
      where_ $ nullify (dom_id dom) ==. d_domain (d_table d)
      pure $ dom_rank dom

  let t = d_table d
  pure $ SearchResult
    { sr_ranking = rank (d_search d) (lit q') rLENGTH
    , sr_popularity = maybeTable null id popularity
    , sr_id      = d_docId t
    , sr_uri     = d_uri   t
    , sr_size    = d_wordCount t
    , sr_title   = d_title t
    , sr_stats   = d_stats t
    }
  where
    q' = compileQuery q


data IL
  = Match Tsquery
  | Full (Query (Document' Expr))


compile' :: Search Text -> IL
compile' (Term txt) = Match $ TsqTerm txt
compile' (Phrase []) = Match $ TsqTerm ""
compile' (Phrase txts) = Match $ foldr1 TsqPhrase $ fmap TsqTerm txts
compile' (Negate se) =
  case compile' se of
    Match ts -> Match $ TsqNot ts
    Full qu -> Full $ except (each documentSchema') qu
compile' (And lhs rhs) = merge TsqAnd intersect (compile' lhs) (compile' rhs)
compile' (Or lhs rhs) = merge TsqOr union (compile' lhs) (compile' rhs)
compile' (SiteLike t) = Full $ do
  d <- each documentSchema'
  where_ $ like (lit $ "%" <> t <> "%") (d_uri $ d_table d)
       &&. d_flags (d_table d) ==. lit mempty
  pure d
compile' (WithProperty prop op) = Full $ do
  d <- each documentSchema'
  where_ $ compileOp op $ getProp prop d
  pure d


getProp :: SiteProp -> Document' Expr -> Expr Int32
getProp JSBundle  = ps_js  . d_stats . d_table
getProp CSSBundle = ps_css . d_stats . d_table


compileOp :: Predicate -> Expr Int32 -> Expr Bool
compileOp (Exactly n) ex     = ex ==. fromIntegral n
compileOp (LessThan n) ex    = ex <.  fromIntegral n
compileOp (GreaterThan n) ex = ex >.  fromIntegral n


merge
    :: (Tsquery -> Tsquery -> Tsquery)
    -> (Query (Document' Expr) -> Query (Document' Expr) -> Query (Document' Expr))
    -> IL
    -> IL
    -> IL
merge t _ (Match t1) (Match t2) = Match $ t t1 t2
merge _ q (Match t1) (Full q2) = Full $ q (matching t1) q2
merge _ q (Full q1) (Match t2) = Full $ q q1 (matching t2)
merge _ q (Full q1) (Full q2) = Full $ q q1 q2


matching :: Tsquery -> Query (Document' Expr)
matching q = do
  d <- each documentSchema'
  where_ $ match (d_search d) (lit q)
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
compileQuery (WithProperty _ _) = TsqTerm ""

