-- |
-- Module: KonBoard.TestLogger
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.TestLogger
  ( basicLogging
  ) where

import Control.Monad.Logger
  (LoggingT, filterLogger, runStderrLoggingT, LogLevel(..))
import Control.Monad.Trans (MonadIO)

basicLogging :: MonadIO m => LoggingT m a -> m a
basicLogging act = runStderrLoggingT $ filterLogger f act
  where
    f _ level = level >= LevelWarn
