{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- https://stackoverflow.com/questions/34624469/crud-pattern-on-haskell-persistent
module Api.Todo where

import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, toSqlKey, insert,
                                              selectFirst, selectList, (==.), deleteWhere, replace)

--import Database.Persist.Class.PersistStore (delete)

import           Servant

import           Config                      (AppT (..))
import           Control.Monad.Metrics       (increment)
import           Models                      (Todo (Todo), runDb, todoCompleted,
                                              todoDescription)
import qualified Models                      as Md

type TodoAPI = "api" :> "todos" :> Get '[JSON] [Entity Todo]
    :<|> "api" :> "todo" :> Capture "id" Int64 :> Get '[JSON] (Entity Todo)
    :<|> "api" :> "todo" :> Capture "id" Int64 :> Delete '[JSON] ()
    :<|> "api" :> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] Int64
    :<|> "api" :> "todo" :> Capture "id" Int64  :> ReqBody '[JSON] Todo :> Put '[JSON] ()

todoApi :: Proxy TodoAPI
todoApi = Proxy

-- | The server that runs the TodoAPI
todoServer :: MonadIO m => ServerT TodoAPI (AppT m)
todoServer = allTodos :<|> singleTodo :<|> deleteTodo :<|> createTodo :<|> updateTodo

-- | Returns all todos in the database.
allTodos :: MonadIO m => AppT m [Entity Todo]
allTodos = do
  increment "allTodos"
  logDebugNS "web" "allTodos"
  runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleTodo :: MonadIO m => Int64 -> AppT m (Entity Todo)
singleTodo id = do
  increment "singleTodo"
  logDebugNS "web" "singleTodo"
  maybeTodo <- runDb (selectFirst [Md.TodoId ==. toSqlKey id] [])
  case maybeTodo of
    Nothing     -> throwError err404
    Just person -> return person

-- | Creates a to-do in the database.
createTodo :: MonadIO m => Todo -> AppT m Int64
createTodo p = do
  increment "createTodo"
  logDebugNS "web" "creating a todo"
  newTodo <- runDb (insert (Todo (todoCompleted p) (todoDescription p)))
  return $ fromSqlKey newTodo

deleteTodo :: MonadIO m => Int64 -> AppT m ()
deleteTodo id = do
  increment "deleteTodo"
  logDebugNS "web" "deleting a todo"
  runDb (deleteWhere [Md.TodoId ==. toSqlKey id]) -- TODO simple `delete`
--  runDb (delete (toSqlKey id))

updateTodo :: MonadIO m => Int64 -> Todo -> AppT m ()
updateTodo id body = do
  increment "deleteTodo"
  logDebugNS "web" "deleting a todo"
  runDb (replace (toSqlKey id) (Todo (todoCompleted body) (todoDescription body)))