import ClassyPrelude
import Test.Hspec
import System.Environment
import Database.PostgreSQL.Simple
import qualified Misc.Client as RW
import qualified Lib
import Control.Concurrent

import Spec.Common
import qualified Spec.User as User
import qualified Spec.Article as Article
import qualified Spec.Comment as Comment

import Data.ByteString.Internal (packChars)

main :: IO ()
main = withEnv . hspec $ do
  User.spec
  Article.spec
  Comment.spec

withEnv :: IO () -> IO ()
withEnv = bracket startEnv cleanEnv . const

startEnv :: IO ThreadId
startEnv = do
  env <- lookupEnv "DATABASE_URL"
  let jdbcConn = fromMaybe "postgresql://127.0.0.1" env
  execPGQuery jdbcConn ["TRUNCATE articles, comments, favorites, followings, users CASCADE;"]
  setEnv "ENABLE_HTTPS" "False"
  setEnv "JWT_EXPIRATION_SECS" "8" -- FIXME: doesn't seem to work
  setEnv "MIGRATION_DIRECTORY" "./postgresql"
  setEnv "JWK_PATH" "./secrets/jwk.sig"
  tId <- forkIO Lib.main
  unlessM healthCheck $ do
    putStrLn "Waiting for server ..."
    threadDelay 1000000
  return tId
  where
    healthCheck =
      either (const False) id <$> runClient RW.health

cleanEnv :: ThreadId -> IO ()
cleanEnv tId = do
  killThread tId
  putStrLn $ "Sever killed (" <> tshow tId <> ")"

execPGQuery :: String -> [Query] -> IO ()
execPGQuery jdbcUrl qrys =
  bracket acquire release execQuery
  where
    acquire = connectPostgreSQL (packChars jdbcUrl)
    release = close
    execQuery conn = forM_ qrys (void . execute_ conn)