module Feature.Todo.Service where

import ClassyPrelude
import Feature.Todo.Types
import Feature.Auth.Types

createTodo :: (TodoRepo m) => CurrentUser -> CreateTodo -> m (())
createTodo (_, userId) = addTodo userId

class (Monad m) => TodoRepo m where
  addTodo :: UserId -> CreateTodo -> m ()

class (Monad m) => TimeRepo m where
  currentTime :: m UTCTime