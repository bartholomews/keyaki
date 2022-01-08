{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Housekeeping where

import Api.Entry (EntriesAPI)
import Api.User (UserAPI)
import Config (AppT (..))
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad.Logger (logDebugNS)
import Control.Monad.Metrics
  ( increment,
    metricsCounters,
  )
import qualified Control.Monad.Metrics as Metrics
import Data.HashMap.Lazy (HashMap)
import Data.IORef (readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Lens.Micro ((^.))
import Servant
import Servant.JS (vanillaJS, writeJSForAPI)
import qualified System.Metrics.Counter as Counter

type HousekeepingAPI =
  "metrics" :> Get '[JSON] (HashMap Text Int64)

housekeepingApi :: Proxy HousekeepingAPI
housekeepingApi = Proxy

-- | The server that runs the UserAPI
housekeepingServer :: MonadIO m => ServerT HousekeepingAPI (AppT m)
housekeepingServer = waiMetrics

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
  increment "metrics"
  logDebugNS "web" "metrics"
  allMetrics <- Metrics.getMetrics
  liftIO $ mapM Counter.read =<< readIORef (allMetrics ^. metricsCounters)

-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript = do
  writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./client/generated/users.js"
  writeJSForAPI (Proxy :: Proxy EntriesAPI) vanillaJS "./client/generated/entries.js"
