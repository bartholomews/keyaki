{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module UserDbSpec where

import           Test.Hspec

import           Control.Exception           (throwIO)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.Reader        (runReaderT)

import           Database.Persist.Postgresql (Entity (..), deleteWhere, insert,
                                              runSqlPool)
import           Database.Persist.Sql        (ConnectionPool)
import           Database.Persist.Types      (Filter)

import           Api.User
import           Config                      (App, AppT (..), Config (..),
                                              Environment (..), envConnPool)
import           Control.Monad.Metrics       (initialize)
import qualified Data.Text                   as T
import           Logger                      (defaultLogEnv)
import           Models

runAppToIO :: Config -> App a -> IO a
runAppToIO config app = do
  result <- runExceptT $ runReaderT (runApp app) config
  case result of
    Left err -> throwIO err
    Right a  -> return a

setupTeardown :: (Config -> IO a) -> IO ()
setupTeardown runTestsWith = do
  env <- defaultLogEnv
  pool <- envConnPool Test env
  metrics <- initialize
  migrateDb pool
  runTestsWith $ Config {configPool = pool, configEnv = Test, configMetrics = metrics, configLogEnv = env}
  cleanDb pool
  where
    migrateDb :: ConnectionPool -> IO ()
    migrateDb = runSqlPool doMigrations
    cleanDb :: ConnectionPool -> IO ()
    cleanDb = deleteAllUsers
    deleteAllUsers :: ConnectionPool -> IO ()
    deleteAllUsers pool = flip runSqlPool pool $ deleteWhere ([] :: [Filter User])

--    pool <- makePool Test env
-- for more detail, see `src/Config.hs`, but this assumes you have...
--   1. a Postgres `test` user
--   2. a `keyaki-test` DB
spec :: Spec
spec =
  around setupTeardown $
  describe "User" $
  it "singleUser fetches User by name" $ \config -> do
    let user = User (T.pack "username") (T.pack "email")
    dbUser <-
      runAppToIO config $ do
        runDb $ insert user
        Entity _ user <- singleUser (T.pack "username")
        return user
    dbUser `shouldBe` user
