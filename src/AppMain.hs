{-# LANGUAGE LambdaCase #-}

module AppMain where

import Options.Applicative
import qualified Search
import qualified Spider
import qualified Purge
import qualified Metric

data Command
  = SearchCommand
  | SpiderCommand
  | PurgeCommand
  | MetricCommand
  deriving (Eq, Ord, Show, Enum, Bounded)


sub :: Parser Command
sub = subparser $ mconcat
  [ command "search" $ info (pure SearchCommand) $ mconcat
      [ progDesc "Start the search server"
      ]
  , command "spider" $ info (pure SpiderCommand) $ mconcat
      [ progDesc "Start the spider"
      ]
  , command "purge" $ info (pure PurgeCommand) $ mconcat
      [ progDesc "Prune webpages that are now excluded by filter rules"
      ]
  , command "root-distance" $ info (pure MetricCommand) $ mconcat
      [ progDesc "Rerun the root-distance algorithm"
      ]
  ]


commandParser :: ParserInfo Command
commandParser =
  info (helper <*> versionOption <*> sub) $ mconcat
    [ fullDesc
    , header "lapse - make programming timelapses"
    ]


versionOption :: Parser (a -> a)
versionOption = infoOption "1.0" $ long "version" <> help "Show version"


main :: IO ()
main = do
  execParser commandParser >>= \case
     SearchCommand -> Search.main
     SpiderCommand -> Spider.spiderMain
     PurgeCommand -> Purge.main
     MetricCommand -> Metric.metricMain

