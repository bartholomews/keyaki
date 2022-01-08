{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Config (AppT (..))
import Control.Monad.Except (MonadIO)
import Control.Monad.Logger (logDebugNS)
import Control.Monad.Metrics (increment)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Postgresql
  ( Entity (..),
    fromSqlKey,
    insert,
    selectFirst,
    selectList,
    (==.),
  )
import Models
  ( User (User),
    runDb,
    userEmail,
    userName,
  )
import qualified Models as Md
import Servant

type UserAPI =
  "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" Text :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

userAPI :: Proxy UserAPI
userAPI = Proxy

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser -- :<|> waiMetrics

-- | Returns all users in the database.
allUsers :: MonadIO m => AppT m [Entity User]
allUsers = do
  increment "allUsers"
  logDebugNS "web" "allUsers"
  runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: MonadIO m => Text -> AppT m (Entity User)
singleUser str = do
  increment "singleUser"
  logDebugNS "web" "singleUser"
  maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
  case maybeUser of
    Nothing -> throwError err404
    Just person -> return person

-- | Creates a user in the database.
createUser :: MonadIO m => User -> AppT m Int64
createUser p = do
  increment "createUser"
  logDebugNS "web" "creating a user"
  newUser <- runDb (insert (User (userName p) (userEmail p)))
  return $ fromSqlKey newUser
