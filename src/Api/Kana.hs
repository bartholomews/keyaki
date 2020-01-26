{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- https://stackoverflow.com/questions/34624469/crud-pattern-on-haskell-persistent
module Api.Kana where

import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, toSqlKey, insert,
                                              selectFirst, selectList, (==.), deleteWhere, replace)

--import Database.Persist.Class.PersistStore (delete)

import           Servant

import           Config                      (AppT (..))
import           Control.Monad.Metrics       (increment)
import           Models                      (Kana (Kana), runDb, kanaCompleted,
                                              kanaDescription)
import qualified Models                      as Md

type KanaAPI = "api" :> "kanas" :> Get '[JSON] [Entity Kana]
    :<|> "api" :> "kana" :> Capture "id" Int64 :> Get '[JSON] (Entity Kana)
    :<|> "api" :> "kana" :> Capture "id" Int64 :> Delete '[JSON] ()
    :<|> "api" :> "kana" :> ReqBody '[JSON] Kana :> Post '[JSON] Int64
    :<|> "api" :> "kana" :> Capture "id" Int64  :> ReqBody '[JSON] Kana :> Put '[JSON] ()

kanaApi :: Proxy KanaAPI
kanaApi = Proxy

-- | The server that runs the KanaAPI
kanaServer :: MonadIO m => ServerT KanaAPI (AppT m)
kanaServer = allKanas :<|> singleKana :<|> deleteKana :<|> createKana :<|> updateKana

-- | Returns all Kanas in the database.
allKanas :: MonadIO m => AppT m [Entity Kana]
allKanas = do
  increment "allKanas"
  logDebugNS "web" "allKanas"
  runDb (selectList [] [])

-- | Returns a Kana by name or throws a 404 error.
singleKana :: MonadIO m => Int64 -> AppT m (Entity Kana)
singleKana id = do
  increment "singleKana"
  logDebugNS "web" "singleKana"
  maybeKana <- runDb (selectFirst [Md.KanaId ==. toSqlKey id] [])
  case maybeKana of
    Nothing     -> throwError err404
    Just kana -> return kana

-- | Creates a Kana in the database.
createKana :: MonadIO m => Kana -> AppT m Int64
createKana p = do
  increment "createKana"
  logDebugNS "web" "creating a Kana"
  newKana <- runDb (insert (Kana (kanaCompleted p) (kanaDescription p)))
  return $ fromSqlKey newKana

deleteKana :: MonadIO m => Int64 -> AppT m ()
deleteKana id = do
  increment "deleteKana"
  logDebugNS "web" "deleting a kana"
  runDb (deleteWhere [Md.KanaId ==. toSqlKey id]) -- TODO simple `delete`
--  runDb (delete (toSqlKey id))

updateKana :: MonadIO m => Int64 -> Kana -> AppT m ()
updateKana id body = do
  increment "deleteKana"
  logDebugNS "web" "deleting a kana"
  runDb (replace (toSqlKey id) (Kana (kanaCompleted body) (kanaDescription body)))