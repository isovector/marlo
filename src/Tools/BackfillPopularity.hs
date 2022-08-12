{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumDecimals #-}

module Tools.BackfillPopularity where

import           Control.Concurrent (threadDelay)
import           Control.Monad (void)
import           DB
import           Data.Foldable (traverse_)
import           Data.Function (fix)
import           Data.Functor.Contravariant ((>$<))
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

--   void $ flip fix 0 $ \loop n -> do
--     let pagesize = 10
--     Right res <- doUpdate conn $ Update
--     -- putStrLn $ showUpdate $ Update
--       { target = documentSchema
--       , from = fmap fst $ limit pagesize $ offset (n * pagesize) $ orderBy (snd >$< asc) $ do
--           dom <- each domainsSchema
--           pure (dom, nullable 1e9 id $ dom_rank dom)
--       , set = \dom doc -> do
--           doc { d_domain = nullify $ dom_id dom }
--       , updateWhere = \dom doc ->
--           d_domain doc ==. null &&. like (lit "%" <>. substring (dom_domain dom) (lit 9) <>. lit "/%") (d_uri doc)
--       , returning = NumberOfRowsAffected
--       }
--     putStrLn $ "updated " <> show res <> " pages' domains"
--     loop $ n + 1

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

