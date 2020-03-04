import Servant.Auth.Server

import Network.Wai.Handler.Warp
import Servant
import Control.Monad
import Control.Monad.Reader
import System.Cron.Schedule
import qualified Data.Hashable as H

import qualified DBAdapter as DB
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

pollWebsites :: AppM IO ()
pollWebsites = do 
      ws <- DB.exec DB.getWebsites
      mapM_ pollWebsite ws

pollWebsite :: Website -> AppM IO ()
pollWebsite ws = do liftIO $ putStrLn ("Polling: " ++ url ws)
                    h <- liftIO $ H.hash <$> scrapePage (url ws)
                    let wid = idWebsite ws
                    b <- DB.exec $ DB.checkWebsiteHash wid h
                    when b $ do _ <- DB.exec $ DB.updateWebsiteHash wid h
                                liftIO $ putStrLn "Changed"
                                return ()

main :: IO ()
main = do
  conf <- config
  _ <- execSchedule $
        addJob (runReaderT pollWebsites conf) $ pollSchedule conf
  runReaderT DB.initDB conf
  runApp conf