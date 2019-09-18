module Feature.Todo.PG where

import ClassyPrelude
import Feature.Auth.Types
import Feature.Todo.Types
import Platform.PG
import Database.PostgreSQL.Simple
          
addTodo :: PG r m => UserId -> CreateTodo -> m ()
addTodo uId param =
  void . withConn $ \conn -> execute conn qry 
    ( createTodoCompleted param, createTodoDescription param, uId )
  where
    qry = "insert into todos (completed, description, created_at, updated_at, author_id) \
          \values (?, ?, now(), now(), ?)"
