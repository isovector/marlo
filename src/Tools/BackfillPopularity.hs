{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumDecimals #-}

module Tools.BackfillPopularity where

import           Control.Concurrent (threadDelay)
import           DB
import           Data.Function (fix)
import           Data.Int (Int16)
import           Data.Text (Text)
import qualified Data.Text as T
import           Domains (rerankPopularity)
import           Prelude hiding (null)
import           Rel8
import           Utils (random, unsafeURI)


main :: IO ()
main = do
  Right conn <- connect

  putStrLn "slowly gathering domains"
  fix $ \loop -> do
    threadDelay 1e7
    Right [dom] <- doSelect conn $
      limit 1 $ orderBy random $ do
        dom <- each domainsSchema
        where_ $ dom_rank dom ==. null
        pure dom

    putStrLn $ T.unpack $ "checking popularity for " <> dom_domain dom
    let uri = unsafeURI $ T.unpack $ dom_domain dom
    rerankPopularity conn uri (dom_domain dom) >>= \case
       (Left _, _) -> putStrLn "errored! did we hit a captcha?"
       (Right _, _) -> loop


substring :: Expr Text -> Expr Int16 -> Expr Text
substring = function "substring"

