{-# LANGUAGE LambdaCase #-}

module AppMain where

import           Data.Text (Text)
import qualified Index
import qualified Metric
import           Options.Applicative
import qualified Purge
import qualified Search
import qualified Spider
import qualified Tools.BackfillPopularity as BackfillPopularity


data Command
  = SearchCommand
  | SpiderCommand (Maybe Text)
  | PurgeCommand
  | IndexCommand
  | MetricCommand
  | BackfillPopularityCommand
  deriving (Eq, Ord, Show)


sub :: Parser Command
sub = subparser $ mconcat
  [ command "search" $ info (pure SearchCommand) $ mconcat
      [ progDesc "Start the search server"
      ]
  , command "spider" $ info (helper <*> parseSpider) $ mconcat
      [ progDesc "Start the spider"
      ]
  , command "purge" $ info (pure PurgeCommand) $ mconcat
      [ progDesc "Prune webpages that are now excluded by filter rules"
      ]
  , command "reindex" $ info (pure IndexCommand) $ mconcat
      [ progDesc "Reindex every explored site"
      ]
  , command "root-distance" $ info (pure MetricCommand) $ mconcat
      [ progDesc "Rerun the root-distance algorithm"
      ]
  , command "backfill-popularity" $ info (pure BackfillPopularityCommand) $ mconcat
      [ progDesc "Backfill website popularity from the alexa api"
      ]
  ]

parseSpider :: Parser Command
parseSpider =
  SpiderCommand
    <$> optional (strOption $ mconcat
          [ long "exclude"
          , help "A sql LIKE pattern to exclude uris from being indexed"
          ])


commandParser :: ParserInfo Command
commandParser =
  info (helper <*> versionOption <*> sub) $ mconcat
    [ fullDesc
    , header "marlo - search, for humans"
    ]


versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" $ long "version" <> help "Show version"


main :: IO ()
main = do
  execParser commandParser >>= \case
     SearchCommand             -> Search.main
     SpiderCommand exc         -> Spider.spiderMain exc
     PurgeCommand              -> Purge.main
     IndexCommand              -> Index.main
     MetricCommand             -> Metric.metricMain
     BackfillPopularityCommand -> BackfillPopularity.main

