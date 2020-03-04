import Servant.Auth.Server

import Network.Wai.Handler.Warp
import Servant
import Control.Monad
import Control.Monad.Reader
import System.Cron.Schedule
import qualified Data.Hashable as H

import DBAdapter as DB
import Handlers.Account
import Config
import Scraper
import Models.Website
import Models.User

type PublicAPI = LoginAPI 
      :<|> Raw
type ProtectedAPI = "account" :> LoginAPI
type API auths = PublicAPI  
            :<|> (Servant.Auth.Server.Auth auths User :> ProtectedAPI)

protected :: Servant.Auth.Server.AuthResult User -> ServerT ProtectedAPI (AppM Handler)
protected (Servant.Auth.Server.Authenticated user) = accountServer
protected _ = throwAll err401

public:: ServerT PublicAPI (AppM Handler)
public = accountServer
    :<|> serveDirectoryWebApp "www" 

server :: ServerT (API auths) (AppM Handler)
server = public :<|> protected 

api :: Proxy (API '[JWT])
api = Proxy

runApp :: Config -> IO ()
runApp conf = do
      let authApi = (Proxy :: Proxy '[CookieSettings, JWTSettings])
      run 8080 (serveWithContext api (authConf conf) $ hoistServerWithContext api authApi (`runReaderT` conf) server)

pollWebsites :: IO ()
pollWebsites = do ws <- execDB DB.getWebsites
                  mapM_ pollWebsite ws

pollWebsite :: Website -> IO ()
pollWebsite ws = do h <- H.hash <$> scrapePage (url ws)
                    let wid = idWebsite ws
                    b <- execDB $ DB.checkWebsiteHash wid h
                    when b $ do _ <- execDB $ DB.updateWebsiteHash wid h
                                -- notify
                                return ()

main :: IO ()
main = do
  conf <- config
  _ <- execSchedule $ do
        addJob pollWebsites $ pollSchedule conf 
  runReaderT initDB conf
  runApp conf