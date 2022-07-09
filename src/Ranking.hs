{-# LANGUAGE NamedFieldPuns #-}
module Ranking where

import Types
import Data.Bool (bool)
import Data.Maybe (isJust)


rank :: Stuff -> Int
rank
  Stuff {s_num_links, s_num_gifs, s_num_images, s_num_scripts, s_author, s_ratio = (para, heads),
         s_sticky, s_numTweets, s_bootstrap, s_googleAnalytics, s_googleAds,
         s_num_forms, s_perc_roman}
  = sum
    [ bool 0 (-1000) $ s_perc_roman < 0.9
    , bool 0 (-100) s_sticky
    , s_num_images * (-1)
    , s_num_gifs * (-10)
    , s_numTweets * (-3)
    , s_num_scripts * (-8)
    , bool 0 10 $ isJust s_author
    , bool 0 (-10) s_bootstrap
    , bool 0 (-5) s_googleAnalytics
    , bool 0 (-25) s_googleAds
    , s_num_links * (-1)
    , round $ (fromIntegral para / fromIntegral heads) * 5
    ]

