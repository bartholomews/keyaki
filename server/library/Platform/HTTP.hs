module Platform.HTTP
      ( main
      ) where

import ClassyPrelude

import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Cors

import qualified Feature.Auth.HTTP as Auth
import qualified Feature.User.HTTP as User
import qualified Feature.Comment.HTTP as Comment
import qualified Feature.Article.HTTP as Article

import System.Environment

type App r m = (Article.Service m, Auth.Service m, Comment.Service m, User.Service m, MonadIO m)

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
  port <- acquirePort
  mayTLSSetting <- acquireTLSSetting
  case mayTLSSetting of
    Nothing ->
      scottyT port runner routes
    Just tlsSetting -> do
      app <- scottyAppT runner routes
      runTLS tlsSetting (setPort port defaultSettings) app
  where
    acquirePort = do
      port <- fromMaybe "" <$> lookupEnv "PORT"
      return . fromMaybe 3000 $ readMay port
    acquireTLSSetting = do
      env <- (>>= readMay) <$> lookupEnv "ENABLE_HTTPS"
      let enableHttps = fromMaybe False env -- TODO [FB] should default to True
      return $ if enableHttps
        then Just $ tlsSettings "server/secrets/tls/certificate.pem" "server/secrets/tls/key.pem"
        else Nothing



-- * Routing

routes :: (App r m) => ScottyT LText m ()
routes = do
  -- middlewares
  middleware $ cors $ const $ Just simpleCorsResourcePolicy
    { corsRequestHeaders = "Authorization":simpleHeaders
    , corsMethods = "PUT":"DELETE":simpleMethods
    }
  options (regex ".*") $ return ()

  -- err
  defaultHandler $ \str -> do
    status status500
    json str

  -- feature routes
  User.routes
  Article.routes
  Comment.routes

  -- health
  get "/api/health" $
    json True

  get (regex "(.*).(js|css)") $ do
    asset <- param "0"
    file $ mconcat ["client/dist/", asset]

-- > get (regex "^/f(.*)r$") $ do
-- >    path <- param "0"
-- >    cap <- param "1"
-- >    text $ mconcat ["Path: ", path, "\nCapture: ", cap]
--
-- >>> curl http://localhost:3000/foo/bar
-- Path: /foo/bar
-- Capture: oo/ba

  get (function $ (\_ -> Just [])) $ file "client/dist/index.html"
--    case pathInfo req of
--         "api":_ -> Nothing
--          _ -> Just []) $ file "ElmFeFMI/index.html"

--  get (function $ \req -> Just [("version", T.pack $ show $ httpVersion req)]) $ do
--      v <- param "version"
--      text v
