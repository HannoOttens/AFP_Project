module App where

import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Control.Monad.Reader
import Control.Monad.State

import Handlers.Account
import Handlers.Targets
import Handlers.Notification
import Config
import Models.User

type PublicAPI = LoginAPI 
type ProtectedAPI = TargetsAPI 
               :<|> "notification" :> NotificationAPI
type RawFiles = Raw
type API = PublicAPI  
            :<|> Servant.Auth.Server.Auth '[Cookie] User :> ProtectedAPI
            :<|> RawFiles

rawFiles :: ServerT RawFiles (AppConfig Handler)
rawFiles = serveDirectoryWebApp "www"

protected :: Servant.Auth.Server.AuthResult User -> ServerT ProtectedAPI (AppConfig Handler)    
protected (Servant.Auth.Server.Authenticated user) = hoistServer protectedAPI (`evalStateT` user) (targetServer :<|> notificationServer)
protected _ = throwAll err401

public:: ServerT PublicAPI (AppConfig Handler)
public = accountServer

server :: ServerT API (AppConfig Handler)
server = public 
      :<|> protected 
      :<|> rawFiles

api :: Proxy API 
api = Proxy

protectedAPI :: Proxy ProtectedAPI
protectedAPI = Proxy 

authApi :: Proxy '[CookieSettings, JWTSettings]
authApi = Proxy

runApp :: Config -> IO ()
runApp conf = run 8080 $ app conf

app :: Config -> Application
app conf = (serveWithContext api (authConf conf) $ hoistServerWithContext api authApi (`runReaderT` conf) server)
