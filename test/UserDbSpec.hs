{-# LANGUAGE DataKinds                  #-}

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
  pool <- envConnPool Test env -- TODO: 
  --  pool <- makePool Test env
  --  should start and teardown a local db, see:
  --  https://hackage.haskell.org/package/testcontainers-0.2.0.0/docs/TestContainers-Docker.html#g:6: 
  metrics <- initialize
  migrateDb pool
  _ <- runTestsWith $ Config {configPool = pool, configEnv = Test, configMetrics = metrics, configLogEnv = env}
  cleanDb pool
  where
    migrateDb :: ConnectionPool -> IO ()
    migrateDb = runSqlPool doMigrations
    cleanDb :: ConnectionPool -> IO ()
    cleanDb = deleteAllUsers
    deleteAllUsers :: ConnectionPool -> IO ()
    deleteAllUsers pool = flip runSqlPool pool $ deleteWhere ([] :: [Filter User])

spec :: Spec
spec =
  around setupTeardown $
  describe "User" $
  it "singleUser fetches User by name" $ \config -> do
    let user = User (T.pack "username") (T.pack "email")
    dbUser <-
      runAppToIO config $ do
        _ <- runDb $ insert user
        Entity _ usr <- singleUser (T.pack "username")
        return usr
    dbUser `shouldBe` user
