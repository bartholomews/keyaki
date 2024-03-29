{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import Control.Concurrent (ThreadId)
import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger (..), runNoLoggingT, runStdoutLoggingT, MonadLoggerIO (..))
import Control.Monad.Metrics
  ( Metrics,
    MonadMetrics,
    getMetrics,
  )
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    asks,
  )
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Postgresql
  ( ConnectionPool,
    ConnectionString,
    createPostgresqlPool,
  )
import qualified Katip as K
import Logger
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant (ServerError)
import System.Environment (lookupEnv)

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a = AppT
  {runApp :: ReaderT Config (ExceptT ServerError m) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
  { configPool :: ConnectionPool,
    configEnv :: Environment,
    configMetrics :: Metrics,
    configEkgServer :: ThreadId,
    configLogEnv :: LogEnv,
    configPort :: Port
  }

instance Monad m => MonadMetrics (AppT m) where
  getMetrics = asks Config.configMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => K.Katip (AppT m) where
  getLogEnv = asks configLogEnv
  localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog = Logger.adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (K.KatipT m) where
  monadLoggerLog = Logger.adapt logMsg

-- deriving instance MonadLoggerIO (KatipT IO)
--instance MonadLogger IO where
--  monadLoggerLog a b c d = do
--    logEnv <- defaultLogEnv
--    runKatipT logEnv $ adapt logMsg a b c d

-- https://github.com/Reykudo/qualifier-bot/blob/4cfbd5787f5515219f4b612a4d8885422eba793d/src/Logger.hs
instance (MonadIO m) => MonadLoggerIO (KatipT m) where
  askLoggerIO = do
    logEnv <- getLogEnv
    pure (\a b c d -> runKatipT logEnv $ monadLoggerLog a b c d)

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
requestLogger :: Environment -> Middleware
requestLogger Test = id
requestLogger Development = logStdoutDev
requestLogger Production = logStdout

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> IO ConnectionPool
makePool Test =
  runNoLoggingT $ createPostgresqlPool (connStr "-test") (envPool Development)
makePool Development =
  runStdoutLoggingT $ createPostgresqlPool (connStr "") (envPool Development)
makePool Production = do
  -- This function makes heavy use of the 'MaybeT' monad transformer, which
  -- might be confusing if you're not familiar with it. It allows us to
  -- combine the effects from 'IO' and the effect of 'Maybe' into a single
  -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
  -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
  -- give us a @Maybe a@, which would make the code quite a bit more
  -- verbose.
  pool <- runMaybeT $ do
    let keys =
          [ "host=",
            "port=",
            "user=",
            "password=",
            "dbname="
          ]
        envs =
          [ "SRS_PG_HOST",
            "SRS_PG_PORT",
            "SRS_PG_USER",
            "SRS_PG_PASSWORD",
            "SRS_PG_DATABASE"
          ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    logEnv <- MaybeT $ fmap Just Logger.defaultLogEnv
    let envConnStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
    lift $ runKatipT logEnv $ createPostgresqlPool envConnStr (envPool Production)
    -- lift $ runStdoutLoggingT $ createPostgresqlPool envConnStr (envPool Production)
  case pool of
    -- If we don't have a correct database configuration, we can't
    -- handle that in the program, so we throw an IO exception. This is
    -- one example where using an exception is preferable to 'Maybe' or
    -- 'Either'.
    Nothing -> throwIO (userError "Database Configuration not present in environment.")
    Just a -> return a

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=keyaki" <> sfx <> " user=postgres password=postgres port=5432"
