{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Common where

import           Config                 (AppT (..))
import           Control.Monad.Except   (MonadIO, liftIO)
import           Control.Monad.Logger   (logDebugNS)
import           Control.Monad.Metrics  (increment, metricsCounters)
import qualified Control.Monad.Metrics  as Metrics
import           Data.HashMap.Lazy      (HashMap)
import           Data.Int               (Int64)
import           Data.IORef             (readIORef)
import           Data.Text              (Text)
import           Lens.Micro             ((^.))
import qualified System.Metrics.Counter as Counter

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
  increment "metrics"
  logDebugNS "web" "metrics"
  metr <- Metrics.getMetrics
  liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)
