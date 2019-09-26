module Platform.PG where

import           ClassyPrelude
import           Data.ByteString.Internal             (packChars)
import           Data.Has
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           System.Environment

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
  dbConfig <- extractDbConfig
  createPool (connectPostgreSQL dbConfig) close 1 10 10

extractDbConfig :: IO ByteString
extractDbConfig = do
  dbUser <- lookupEnv "PG_USER"
  dbPassword <- lookupEnv "PG_PASSWORD"
  dbHost <- lookupEnv "PG_HOST"
  dbPort <- lookupEnv "PG_PORT"
  dbTable <- lookupEnv "PG_DATABASE"
  let user = fromMaybe "postgres" dbUser
  let password =
        case dbPassword of
          Nothing     -> ""
          Just secret -> ":" ++ secret
  let host = fromMaybe "0.0.0.0" dbHost ++ ":" ++ fromMaybe "5432" dbPort
  let table = fromMaybe "keyaki" dbTable
  return (packChars $ "postgresql://" ++ user ++ password ++ "@" ++ host ++ "/" ++ table)

migrateDb :: String -> Pool Connection -> IO ()
migrateDb migrationDirectory pool = withResource pool $ \conn -> void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationCommands [MigrationInitialization, MigrationDirectory migrationDirectory]

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withResource pool action
