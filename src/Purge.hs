{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wall              #-}

module Purge where

import DB
import Hasql.Connection (acquire)
import Hasql.Session
import Rel8
import Signals (forbidPaths)
import qualified Data.Text as T

main :: IO ()
main = do
  Right conn <- acquire connectionSettings
  Right n <- flip run conn $ statement () $ delete $ Delete
    { from = discoverySchema
    , using = pure ()
    , deleteWhere = \_ d -> do
        foldr1 (||.) $ do
          z <- forbidPaths
          pure $ like (lit $ T.pack $ "%" <> z <> "%") $ d_uri d
    , returning = NumberOfRowsAffected
    }
  putStrLn $ "deleted " <> show n <> " rows"
  pure ()

