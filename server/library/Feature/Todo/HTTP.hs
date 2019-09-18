module Feature.Todo.HTTP
      ( routes
      , Service(..)
      ) where

import ClassyPrelude hiding (delete)

import Feature.Todo.Types
import Feature.Auth.Types
import Feature.Common.HTTP
import qualified Feature.Auth.HTTP as Auth
import Web.Scotty.Trans
--import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))

class Monad m => Service m where
  createTodo :: CurrentUser -> CreateTodo -> m ()


routes :: (Auth.Service m, Service m, MonadIO m) => ScottyT LText m ()
routes =

  post "/api/todo" $ do
    curUser <- Auth.requireUser
    req <- parseJsonBody ("todo" .: createTodoForm)
    result <- lift $ createTodo curUser req
    json $ TodoWrapper result


-- * Errors

--todoErrorHandler :: (ScottyError e, Monad m) => TodoError -> ActionT e m ()
--todoErrorHandler err = case err of
--  TodoErrorNotFound _ -> do
--    status status404
--    json err
--  TodoErrorNotAllowed _ -> do
--    status status403
--    json err

-- * Forms

createTodoForm :: (Monad m) => DF.Form [Text] m CreateTodo
createTodoForm = CreateTodo <$> "completed" .: DF.bool Nothing
                                  <*> "description" .: DF.text Nothing

