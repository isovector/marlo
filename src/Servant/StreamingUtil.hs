module Servant.StreamingUtil where

import Control.Monad.Reader
import Servant.Types.SourceT

data Emit m o = forall b. Emit
  { unEmit :: StepT m o -> m b
  }

newtype Streaming o m a = Streaming
  { unStreaming :: ReaderT (Emit m o) m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Streaming o) where
  lift = Streaming . lift


streamingToSourceT :: Monad m => Streaming o m () -> SourceT m o
streamingToSourceT s = SourceT $ \emit -> do
  flip runReaderT (Emit emit) $ unStreaming s
  emit Stop


yield :: Monad m => o -> Streaming o m ()
yield o =
  Streaming ask >>= \case
    Emit emit -> do
      Streaming $ lift $ void $ emit $ Yield o Stop

