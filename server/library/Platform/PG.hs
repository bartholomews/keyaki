module Platform.PG where

import ClassyPrelude
import Data.Has
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Environment

type Env = Pool Connection

type PG r m = (MonadReader r m, Has Env r, MonadIO m)
    
init :: IO Env
init = do
  pool <- acquirePool
  envUrl <- lookupEnv "MIGRATION_DIRECTORY"
  let migrationDirectory = fromMaybe "server/postgresql" envUrl
  migrateDb migrationDirectory pool
  return pool

acquirePool :: IO (Pool Connection)
acquirePool = do
  envUrl <- lookupEnv "DATABASE_URL"
  let pgUrl = fromString $ fromMaybe "postgresql://0.0.0.0/keyaki" envUrl
  createPool (connectPostgreSQL pgUrl) close 1 10 10

migrateDb :: String -> Pool Connection -> IO ()
migrateDb migrationDirectory pool = withResource pool $ \conn ->
  void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationCommands [ MigrationInitialization, MigrationDirectory migrationDirectory ]

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withResource pool action
