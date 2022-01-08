{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- https://stackoverflow.com/questions/34624469/crud-pattern-on-haskell-persistent
module Api.Entry where

import Config (AppT (..))
import Control.Monad.Except (MonadIO)
import Control.Monad.Logger (logDebugNS)
import Control.Monad.Metrics (increment)
import Data.Aeson.Types (FromJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Postgresql
  ( Entity (..),
    deleteWhere,
    insertEntity,
    replace,
    selectFirst,
    selectList,
    toSqlKey,
    (==.),
  )
import GHC.Generics
import Models
  ( Entry (Entry),
    entryActive,
    entryKana,
    entryMeaning,
    runDb,
  )
import qualified Models as Md
import Servant

type EntriesAPI =
  "entries" :> Get '[JSON] [Entity Entry]
    :<|> "entries" :> Capture "id" Int64 :> Get '[JSON] (Entity Entry)
    :<|> "entries" :> Capture "id" Int64 :> Delete '[JSON] ()
    :<|> "entries" :> ReqBody '[JSON] EntryRequest :> Post '[JSON] (Entity Entry)
    :<|> "entries" :> Capture "id" Int64 :> ReqBody '[JSON] Entry :> Put '[JSON] ()

entriesApi :: Proxy EntriesAPI
entriesApi = Proxy

-- | The server that runs the EntryAPI
entryServer :: MonadIO m => ServerT EntriesAPI (AppT m)
entryServer = allEntries :<|> singleEntry :<|> deleteEntry :<|> createEntry :<|> updateEntry

-- | Returns all entries in the database.
allEntries :: MonadIO m => AppT m [Entity Entry]
allEntries = do
  increment "allEntries"
  logDebugNS "web" "allEntries"
  runDb (selectList [] [])

-- | Returns a entry by name or throws a 404 error.
singleEntry :: MonadIO m => Int64 -> AppT m (Entity Entry)
singleEntry entryId = do
  increment "singleEntry"
  logDebugNS "web" "singleEntry"
  maybeEntry <- runDb (selectFirst [Md.EntryId ==. toSqlKey entryId] [])
  case maybeEntry of
    Nothing -> throwError err404
    Just entry -> return entry

data EntryRequest = EntryRequest
  { kana :: Text,
    meaning :: Text
  }
  deriving (Generic, Show)

instance FromJSON EntryRequest

-- | Creates a entry in the database.
createEntry :: MonadIO m => EntryRequest -> AppT m (Entity Entry)
createEntry req = do
  increment "createEntry"
  logDebugNS "web" "creating an entry"
  runDb (insertEntity (Entry True (kana req) (meaning req)))

deleteEntry :: MonadIO m => Int64 -> AppT m ()
deleteEntry entryId = do
  increment "deleteEntry"
  logDebugNS "web" "deleting an entry"
  runDb (deleteWhere [Md.EntryId ==. toSqlKey entryId]) -- TODO simple `delete`

--  runDb (delete (toSqlKey id))
-- TODO update with optional fields
updateEntry :: MonadIO m => Int64 -> Entry -> AppT m ()
updateEntry entryId body = do
  increment "updateEntry"
  logDebugNS "web" "updating an entry"
  runDb (replace (toSqlKey entryId) (Entry (entryActive body) (entryKana body) (entryMeaning body)))
