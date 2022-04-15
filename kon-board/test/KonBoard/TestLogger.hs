module KonBoard.TestLogger
    ( basicLogging
    ) where

import           Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStderrLoggingT)
import           Control.Monad.Trans  (MonadIO)

basicLogging :: MonadIO m => LoggingT m a -> m a
basicLogging act = runStderrLoggingT $ filterLogger f act
  where
    f _ level = level >= LevelWarn
