module Tools.ImportPopularity where

import           Control.Monad (void)
import           DB
import           Data.Int (Int32)
import           Data.Text (Text)
import qualified Data.Text as T
import           Domains (upsertDomain)
import           Marlo.Robots (fetchRobotDirectives)
import           Rel8 (lit)
import           Streaming.Cassava (HasHeader (NoHeader), defaultDecodeOptions, decodeWithErrors)
import qualified Streaming.Prelude as S
import           Streaming.With (withBinaryFileContents)
import           Utils (hush, unsafeURI)


main :: FilePath -> IO ()
main csv = do
  Right conn <- connect
  void
    $ withBinaryFileContents csv
    $ S.mapM_
      ( \(rank, domnameish) -> do
          let domain = "https://" <> domnameish
              uri = unsafeURI $ T.unpack domain
          rules <- fetchRobotDirectives uri
          putStrLn $ T.unpack $ "importing " <> domnameish
          z <- doInsert conn $ upsertDomain $ (lit emptyDomain)
            { dom_domain = lit domain
            , dom_rules  = lit rules
            , dom_rank   = lit $ Just rank
            }
          case z of
            Left qe -> print qe
            Right _ -> pure ()
      )
    . S.mapMaybe hush
    . decodeWithErrors @_ @(Int32, Text) defaultDecodeOptions NoHeader

