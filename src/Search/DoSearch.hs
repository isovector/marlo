{-# LANGUAGE AllowAmbiguousTypes #-}

module Search.DoSearch where

import           Control.Applicative (liftA2)
import           Control.Monad.IO.Class (liftIO)
import           DB
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Text (Text)
import           Data.Time (NominalDiffTime)
import           Linear (V3)
import qualified Lucid as L
import           Rel8 hiding (max, index)
import           Search.Common (searchPage)
import           Search.Compiler (compileSearch)
import           Search.Machinery
import           Servant
import           Types
import           Utils (paginate, timeItT)


gatherSearch
    :: forall v
     . SearchMethod v
    => Connection
    -> WindowSize
    -> V3 SearchDimension
    -> Search Text
    -> Maybe PageNumber
    -> IO (PageNumber, (NominalDiffTime, (Int64, SearchMethodResult v)))
gatherSearch conn ws dims q mpage = do
  let page = fromMaybe 1 mpage
      pagenum = subtract 1 $ Prelude.max 1 $ maybe 1 getPageNumber mpage
  fmap (page,) $ timeItT $ do
    Right prepped <- do
      doSelect conn $ do
        let x = compileSearch q
        ( case limitStrategy @v of
            Limit n       -> limit n
            Paginate size -> paginate size pagenum
          ) $ liftA2 (,) (countRows x) x

    let cnt = maybe 0 fst $ listToMaybe prepped
        docs = fmap snd prepped
    res <- accumResults @v conn ws dims q docs
    pure (cnt, res)


doSearch
    :: forall v
     . SearchMethod v
    => Connection
    -> WindowSize
    -> V3 SearchDimension
    -> Search Text
    -> Maybe PageNumber
    -> Handler (SourceIO (L.Html ()))
doSearch conn ws dims q mpage = do
  (page, (dur, (cnt, res))) <- liftIO $ gatherSearch @v conn ws dims q mpage
  pure $ searchPage @v conn dims q dur page cnt res

