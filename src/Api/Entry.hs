{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- https://stackoverflow.com/questions/34624469/crud-pattern-on-haskell-persistent
module Api.Entry where

import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), deleteWhere,
                                              insertEntity, replace,
                                              selectFirst, selectList, toSqlKey,
                                              (==.))

--import Database.Persist.Class.PersistStore (delete)
import           Servant

import           Config                      (AppT (..))
import           Control.Monad.Metrics       (increment)
import           Data.Aeson.Types            (FromJSON)
import           Data.Text                   (Text)
import           GHC.Generics
import           Models                      (Entry (Entry), entryActive,
                                              entryKana, entryMeaning, runDb)
import qualified Models                      as Md

type EntryAPI
   = "api" :> "entries" :> Get '[ JSON] [Entity Entry] :<|>
   "api" :> "entry" :> Capture "id" Int64 :> Get '[ JSON] (Entity Entry) :<|>
   "api" :> "entry" :> Capture "id" Int64 :> Delete '[ JSON] () :<|>
   "api" :> "entry" :> ReqBody '[ JSON] EntryRequest :> Post '[ JSON] (Entity Entry) :<|>
   "api" :> "entry" :> Capture "id" Int64 :> ReqBody '[ JSON] Entry :> Put '[ JSON] ()

entryApi :: Proxy EntryAPI
entryApi = Proxy

-- | The server that runs the EntryAPI
entryServer :: MonadIO m => ServerT EntryAPI (AppT m)
entryServer = allEntries :<|> singleEntry :<|> deleteEntry :<|> createEntry :<|> updateEntry

-- | Returns all entries in the database.
allEntries :: MonadIO m => AppT m [Entity Entry]
allEntries = do
  increment "allEntries"
  logDebugNS "web" "allEntries"
  runDb (selectList [] [])

-- | Returns a entry by name or throws a 404 error.
singleEntry :: MonadIO m => Int64 -> AppT m (Entity Entry)
singleEntry id = do
  increment "singleEntry"
  logDebugNS "web" "singleEntry"
  maybeEntry <- runDb (selectFirst [Md.EntryId ==. toSqlKey id] [])
  case maybeEntry of
    Nothing    -> throwError err404
    Just entry -> return entry

data EntryRequest =
  EntryRequest
    { kana :: Text
    , meaning   :: Text
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
deleteEntry id = do
  increment "deleteEntry"
  logDebugNS "web" "deleting an entry"
  runDb (deleteWhere [Md.EntryId ==. toSqlKey id]) -- TODO simple `delete`

--  runDb (delete (toSqlKey id))
-- TODO update with optional fields
updateEntry :: MonadIO m => Int64 -> Entry -> AppT m ()
updateEntry id body = do
  increment "updateEntry"
  logDebugNS "web" "updating an entry"
  runDb (replace (toSqlKey id) (Entry (entryActive body) (entryKana body) (entryMeaning body)))
