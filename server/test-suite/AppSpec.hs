-- test-suite/AppSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import           Test.Hspec
--import           Test.Hspec.Wai             (get, shouldRespondWith, with)

--import           Api
--import           App                        (mkApp)
--import           Control.Exception          (ErrorCall (..), throwIO)
--import           Control.Monad.Trans.Except
--import           Database.Persist
--import           Database.Persist.Sqlite    (toSqlKey)
--import           Models
--import           Network.HTTP.Client
--import           Network.Wai                (Application)
--import           Network.Wai.Handler.Warp
--import           Servant.API
--import           Servant.Client
--import           Test.Mockery.Directory     (inTempDirectory)

--createTodo :: Todo -> Manager -> BaseUrl -> ClientM TodoId
--readTodo :: TodoId -> Manager -> BaseUrl -> ClientM (Maybe (Entity Todo))
--updateTodo :: TodoId -> Todo -> Manager -> BaseUrl -> ClientM NoContent
--deleteTodo :: TodoId -> Manager -> BaseUrl -> ClientM NoContent
--getTodos :: Manager -> BaseUrl -> ClientM [Entity Todo]

--createTodo :<|> readTodo :<|> updateTodo :<|> deleteTodo :<|> getTodos = client api

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True
                  -- do
--  around withApp $ do
--
--    describe "GET /todos" $ do
--      it "responds with an empty list by default" $ \ port -> do
--        try port getTodos `shouldReturn` []
--
--    describe "POST /todo" $ do
--      it "responds with a key of new created todo" $ \ port -> do
--        let todo = Todo "Do something" True
--        id <- try port (createTodo todo)
--        -- todo is first entry, so it has to have an id of "1"
--        id `shouldBe` toSqlKey (read "1")
--
--    describe "GET /todo" $ do
--      it "responds with Nothing if no todo is available" $ \ port -> do
--        let sqlKey = toSqlKey (read "100")
--        try port (readTodo sqlKey) `shouldReturn` Nothing
--      it "responds with a todo" $ \ port -> do
--        let todo = Todo "Do something" True
--        id <- try port (createTodo todo)
--        let entity = Entity id todo
--        try port (readTodo id) `shouldReturn` (Just entity)
--
--
--    describe "DELETE /todo" $ do
--      it "updates a todo" $ \ port -> do
--        let todoA = Todo "Do A" True
--        let todoB = Todo "Do B" True
--        idA <- try port (createTodo todoA)
--        idB <- try port (createTodo todoB)
--        let entityB = Entity idB todoB
--        try port (deleteTodo idA)
--        try port getTodos `shouldReturn` [entityB]
--
--withApp :: (Int -> IO a) -> IO a
--withApp action =
--  inTempDirectory $ do
--    app <- mkApp "sqlite.db"
--    testWithApplication (return app) action
--
--try :: Int -> (Manager -> BaseUrl -> ClientM a) -> IO a
--try port action = do
--  manager <- newManager defaultManagerSettings
--  let baseUrl = BaseUrl Http "localhost" port ""
--  result <- runExceptT $ action manager baseUrl
--  case result of
--    Left err -> throwIO $ ErrorCall $ show err
--    Right a -> return a

main :: IO ()
main = hspec spec

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
--import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
--import Test.Tasty.Hspec

--main :: IO ()
--main = do
--    test <- testSpec "keyaki-skeletron" spec
--    Test.Tasty.defaultMain test