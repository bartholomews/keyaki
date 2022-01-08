{-# LANGUAGE OverloadedStrings #-}

module Init where

import Api (app)
import Api.Housekeeping (generateJavaScript)
import Config
  ( Config (..),
    Environment (..),
    makePool,
    requestLogger,
  )
import Control.Concurrent (killThread)
import Control.Exception (bracket)
import qualified Control.Monad.Metrics as M
import qualified Data.Pool as Pool
import Database.Persist.Postgresql (runSqlPool)
import qualified Katip as K
import Lens.Micro ((^.))
import Logger (defaultLogEnv)
import Models (doMigrations)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Metrics (metrics, registerNamedWaiMetrics, registerWaiMetrics)
import Safe (readMay)
import System.Environment (lookupEnv)
import System.Remote.Monitoring
  ( forkServer,
    serverMetricStore,
    serverThreadId,
  )

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runApp :: IO ()
runApp = bracket acquireConfig shutdownApp runWithConfig
  where
    runWithConfig cfg = run (configPort cfg) =<< initialize cfg

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize cfg = do
  waiMetrics <- registerNamedWaiMetrics "srs" (configMetrics cfg ^. M.metricsStore)
  let logger = requestLogger (configEnv cfg)
  runSqlPool doMigrations (configPool cfg)
  generateJavaScript
  pure . logger . metrics waiMetrics . app $ cfg

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
  cfgPort <- lookupSetting "PORT" 8081
  cfgEnv <- lookupSetting "ENV" Development
  cfgLogEnv <- defaultLogEnv
  cfgPool <- makePool cfgEnv
  ekgServer <- forkServer "localhost" 8000
  let store = serverMetricStore ekgServer
  _ <- registerWaiMetrics store
  cfgMetrics <- M.initializeWith store
  pure
    Config
      { configPool = cfgPool,
        configEnv = cfgEnv,
        configMetrics = cfgMetrics,
        configLogEnv = cfgLogEnv,
        configPort = cfgPort,
        configEkgServer = serverThreadId ekgServer
      }

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  _ <- K.closeScribes (configLogEnv cfg)
  Pool.destroyAllResources (configPool cfg)
  -- Monad.Metrics does not provide a function to destroy metrics store
  -- so, it'll hopefully get torn down when async exception gets thrown
  -- at metrics server process
  killThread (configEkgServer cfg)
  pure ()

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str = error $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]
