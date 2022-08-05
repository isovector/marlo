{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Config where

import Config.Type
import System.IO.Unsafe (unsafePerformIO)

Config{..} = unsafePerformIO $ fmap read $ readFile "config"

