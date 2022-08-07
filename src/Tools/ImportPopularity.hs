module Tools.ImportPopularity where

import           Control.Monad (void)
import           DB
import           Data.Int (Int32)
import           Data.Text (Text)
import qualified Data.Text as T
import           Domains (upsertDomain)
import           Rel8 (lit)
import           Streaming.Cassava (HasHeader (NoHeader), defaultDecodeOptions, decodeWithErrors)
import qualified Streaming.Prelude as S
import           Streaming.With (withBinaryFileContents)
import           Utils (hush)


main :: FilePath -> IO ()
main csv = do
  Right conn <- connect
  void
    $ withBinaryFileContents csv
    $ S.mapM_
      ( \(rank, domnameish) -> do
          putStrLn $ T.unpack $ "importing " <> domnameish
          doInsert conn $ upsertDomain $ (lit emptyDomain)
            { dom_domain = lit $ "https://" <> domnameish
            , dom_rank   = lit $ Just rank
            }
      )
    . S.mapMaybe hush
    . decodeWithErrors @_ @(Int32, Text) defaultDecodeOptions NoHeader

