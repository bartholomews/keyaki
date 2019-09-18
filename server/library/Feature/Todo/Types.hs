module Feature.Todo.Types where

import ClassyPrelude

import Database.PostgreSQL.Simple.FromRow
import Platform.AesonUtil

type Slug = Text

data Todo = Todo 
  { completed :: Bool
  , description :: Text
  } deriving (Eq, Show)

data CreateTodo = CreateTodo
  { createTodoCompleted :: Bool
  , createTodoDescription :: Text
  } deriving (Eq, Show)
  
data TodoError
  = TodoErrorNotFound Slug
  | TodoErrorNotAllowed Slug
  deriving (Eq, Show)

newtype TodoWrapper a = TodoWrapper { todoWrapperTodo :: a } deriving (Eq, Show)
data TodosWrapper a = TodosWrapper { todosWrapperTodos :: [a], todosWrapperTodosCount :: Int } deriving (Eq, Show)

-- * Instances

$(commonJSONDeriveMany
  [ ''Todo
  , ''CreateTodo
  , ''TodoError
  , ''TodoWrapper
  , ''TodosWrapper
  ])

instance FromRow Todo where
  fromRow = Todo 
    <$> field
    <*> field