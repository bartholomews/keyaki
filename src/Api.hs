{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app
  ) where

import           Control.Monad.Reader            (runReaderT)
import           Servant                         ((:<|>) ((:<|>)),
                                                  Proxy (Proxy), Raw, Server,
                                                  serve, serveDirectoryWith)
import           Servant.Server

import           Api.Todo                        (TodoAPI, todoApi, todoServer)
import           Api.User                        (UserAPI, userApi, userServer)
import           Config                          (AppT (..), Config (..))
import           Data.Text                       (pack)
import           Network.HTTP.Types.Method
import           Network.Wai.Middleware.Cors     (CorsResourcePolicy, cors,
                                                  corsMethods,
                                                  simpleCorsResourcePolicy)

import           Network.Wai                     (Middleware)
import           WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import           WaiAppStatic.Types

-- | This is the function we export to run our 'UserAPI'. Given
-- a 'Config', we return a WAI 'Application' which any WAI compliant server
-- can run.
--userApp :: Config -> Application
--userApp cfg = serve userApi (appToServer cfg)
-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToUserServer :: Config -> Server UserAPI
appToUserServer cfg = hoistServer userApi (convertApp cfg) userServer

appToTodoServer :: Config -> Server TodoAPI
appToTodoServer cfg = hoistServer todoApi (convertApp cfg) todoServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

--  https://github.com/haskell-servant/servant/issues/1195
--  https://www.reddit.com/r/haskellquestions/comments/cbr4mg/serving_static_files_with_haskellservant/
staticSettings :: FilePath -> StaticSettings
staticSettings root = ds {ssLookupFile = lookup}
  where
    ds = defaultFileServerSettings root
    lookup p = do
      f <- ssLookupFile ds p
      case f of
        LRFile f   -> return $ LRFile f
        LRFolder f -> return $ LRFolder f
        LRNotFound -> ssLookupFile ds [unsafeToPiece (pack "index.html")]

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory, with fallback serving `index.html`
static :: Server Raw
static = serveDirectoryWith (staticSettings "client/dist")

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type AppAPI = UserAPI :<|> TodoAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy

-- | Finally, this function takes a configuration and runs our 'UserAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
app :: Config -> Application
app cfg = simpleCors (serve appApi (appToUserServer cfg :<|> appToTodoServer cfg :<|> static))

--  https://github.com/haskell-servant/servant/issues/278
--  https://github.com/haskell-servant/servant/issues/154
--  http://rundis.github.io/blog/2016/haskel_elm_spa_part2.html
simpleCors :: Middleware
simpleCors =
  cors $
  const
    (Just
       simpleCorsResourcePolicy
         {corsMethods = [methodGet, methodHead, methodPost, methodDelete, methodPut, methodOptions]})
