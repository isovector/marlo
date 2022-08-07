{-# LANGUAGE LambdaCase #-}

module AppMain where

import Options.Applicative
import qualified Search
import qualified Spider
import qualified Purge
import qualified Metric
import qualified Index
import Data.Text (Text)

data Command
  = SearchCommand
  | SpiderCommand (Maybe Text)
  | PurgeCommand
  | IndexCommand
  | MetricCommand
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
     SearchCommand -> Search.main
     SpiderCommand exc -> Spider.spiderMain exc
     PurgeCommand -> Purge.main
     IndexCommand -> Index.main
     MetricCommand -> Metric.metricMain

