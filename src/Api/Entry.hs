{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- https://stackoverflow.com/questions/34624469/crud-pattern-on-haskell-persistent
module Api.Entry where

import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, toSqlKey, insert,
                                              selectFirst, selectList, (==.), deleteWhere, replace)

--import Database.Persist.Class.PersistStore (delete)

import           Servant

import           Config                      (AppT (..))
import           Control.Monad.Metrics       (increment)
import           Models                      (Entry (Entry), runDb, entryActive,
                                              entryRomanji)
import qualified Models                      as Md

type EntryAPI = "api" :> "entries" :> Get '[JSON] [Entity Entry]
    :<|> "api" :> "entry" :> Capture "id" Int64 :> Get '[JSON] (Entity Entry)
    :<|> "api" :> "entry" :> Capture "id" Int64 :> Delete '[JSON] ()
    :<|> "api" :> "entry" :> ReqBody '[JSON] Entry :> Post '[JSON] Int64
    :<|> "api" :> "entry" :> Capture "id" Int64  :> ReqBody '[JSON] Entry :> Put '[JSON] ()

entryApi :: Proxy EntryAPI
entryApi = Proxy

-- | The server that runs the EntryAPI
entryServer :: MonadIO m => ServerT EntryAPI (AppT m)
entryServer = allEntries :<|> singleEntry :<|> deleteEntry :<|> createEntry :<|> updateEntry

-- | Returns all Entries in the database.
allEntries :: MonadIO m => AppT m [Entity Entry]
allEntries = do
  increment "allEntries"
  logDebugNS "web" "allEntries"
  runDb (selectList [] [])

-- | Returns a Entry by name or throws a 404 error.
singleEntry :: MonadIO m => Int64 -> AppT m (Entity Entry)
singleEntry id = do
  increment "singleEntry"
  logDebugNS "web" "singleEntry"
  maybeEntry <- runDb (selectFirst [Md.EntryId ==. toSqlKey id] [])
  case maybeEntry of
    Nothing     -> throwError err404
    Just entry -> return entry

-- | Creates a Entry in the database.
createEntry :: MonadIO m => Entry -> AppT m Int64
createEntry p = do
  increment "createEntry"
  logDebugNS "web" "creating a Entry"
  newEntry <- runDb (insert (Entry (entryActive p) (entryRomanji p)))
  return $ fromSqlKey newEntry

deleteEntry :: MonadIO m => Int64 -> AppT m ()
deleteEntry id = do
  increment "deleteEntry"
  logDebugNS "web" "deleting a entry"
  runDb (deleteWhere [Md.EntryId ==. toSqlKey id]) -- TODO simple `delete`
--  runDb (delete (toSqlKey id))

updateEntry :: MonadIO m => Int64 -> Entry -> AppT m ()
updateEntry id body = do
  increment "deleteEntry"
  logDebugNS "web" "deleting a entry"
  runDb (replace (toSqlKey id) (Entry (entryActive body) (entryRomanji body)))