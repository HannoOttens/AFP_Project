{-|
Module      : App
Description : Application manager

Run application and server details.
-}
module App where

import Control.Monad.Reader
import Control.Monad.State
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server

import Config
import Handlers.Account
import Handlers.Notification
import Handlers.Targets
import Models.User

-- | All reachable things without logging in
type PublicAPI = LoginAPI 
-- | URL that can only be reached when logged in
type ProtectedAPI = TargetsAPI 
               :<|> NotificationAPI
-- | All static files
type RawFiles = Raw
-- | Full API type for application
type API = PublicAPI  
            :<|> Servant.Auth.Server.Auth '[Cookie] Session :> ProtectedAPI
            :<|> RawFiles

-- | Server instance for serving the raw files
rawFiles :: ServerT RawFiles (AppConfig Handler)
rawFiles = serveDirectoryWebApp "www"

-- | Server instance for serving the ProtectedAPI
protected :: Servant.Auth.Server.AuthResult Session -> ServerT ProtectedAPI (AppConfig Handler)    
protected (Servant.Auth.Server.Authenticated session) = hoistServer protectedAPI (`evalStateT` userID session) (targetServer :<|> notificationServer)
protected _ = throwAll err401

-- | Server instance for serving the PublicAPI
public:: ServerT PublicAPI (AppConfig Handler)
public = accountServer

-- | Server instance for serving the enitre API
server :: ServerT API (AppConfig Handler)
server = public 
      :<|> protected 
      :<|> rawFiles

-- | Proxy type for API
api :: Proxy API 
api = Proxy

-- | Proxy type for ProtectedAPI
protectedAPI :: Proxy ProtectedAPI
protectedAPI = Proxy 

-- | Proxy type for authentication settings
authApi :: Proxy '[CookieSettings, JWTSettings]
authApi = Proxy

-- | Run server on port 8080
runApp :: Config -> IO ()
runApp conf = run 8080 $ app conf

-- | Create application instance
app :: Config -> Application
app conf = serveWithContext api (authConf conf) 
         $ hoistServerWithContext api authApi (`runReaderT` conf) server
