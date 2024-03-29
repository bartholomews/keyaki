{-# LANGUAGE OverloadedStrings #-}
module Logger
    ( adapt
    , defaultLogEnv
    , logMsg
    , runKatipT
    , KatipT(..)
    , Katip(..)
    , LogEnv
    , Severity(..)
    ) where

import           Control.Monad.Logger
import qualified Control.Monad.Logger   as Logger
import           Katip
import qualified System.IO              as IO
import qualified System.Log.FastLogger  as FastLogger

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
    -- https://github.com/Soostone/katip/blob/master/katip/examples/example.hs
    handleScribe <- Katip.mkHandleScribe ColorIfTerminal IO.stdout (permitItem DebugS) V2
    env <- initLogEnv "srs" "production"
    registerScribe "stdout" handleScribe defaultScribeSettings env

fromLevel :: LogLevel -> Severity
fromLevel LevelDebug     = DebugS
fromLevel LevelInfo      = InfoS
fromLevel LevelWarn      = WarningS
fromLevel LevelError     = ErrorS
fromLevel (LevelOther _) = NoticeS

-- | Transforms Katip logMsg into monadLoggerLog to be used inside
-- MonadLogger monad
adapt :: (ToLogStr msg)  =>
         (Namespace -> Severity -> Katip.LogStr -> m ()) ->
         Loc -> LogSource -> LogLevel -> msg -> m ()
adapt f _ src lvl msg =
    f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Namespace [src]
    -- not sure how fast this is going to be
    logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
