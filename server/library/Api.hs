{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Database.Persist (Entity)
import Models (Todo, TodoId)
import Servant ((:<|>), (:>), Capture, Delete, Get, JSON, NoContent, Post, Put, Raw, ReqBody, Server, serveDirectoryFileServer)

type Api
  -- create
   = "todo" :> ReqBody '[ JSON] Todo :> Post '[ JSON] TodoId
  -- read
      :<|> "todo" :> Capture "key" TodoId :> Get '[ JSON] (Maybe (Entity Todo))
  -- update
      :<|> "todo" :> Capture "key" TodoId :> ReqBody '[ JSON] Todo :> Put '[ JSON] NoContent
  -- delete
      :<|> "todo" :> Capture "id" TodoId :> Delete '[ JSON] NoContent
  -- all
      :<|> "todos" :> Get '[ JSON] [Entity Todo]
      :<|> Raw
      
-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: Server Raw
files = serveDirectoryFileServer "client/dist"

api :: Proxy Api
api = Proxy