module App where

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import qualified DBAdapter as DB
import Handlers.Account
import Handlers.Targets
import Handlers.Notification
import Config
import Scraper
import Models.Website
import Models.User
import Models.Target

type PublicAPI = LoginAPI 
type ProtectedAPI = TargetsAPI 
               :<|> NotificationAPI
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

-- Poll a website, check for changes, update hash, notify if changed and give site content
poll :: Website -> AppConfig IO (Bool, SiteContent)
poll ws = do
      let u = url ws
      let wid = idWebsite ws
      liftIO $ putStrLn ("Polling: " ++ u)
      site <- liftIO $ getSite u
      let h = scrapePage site
      b <- DB.exec $ DB.checkWebsiteHash wid h
      when b $ do
            _ <- DB.exec $ DB.updateWebsiteHash wid h
            return ()
      return (b, site)

-- Poll all website from the database
pollWebsites :: AppConfig IO ()
pollWebsites = do 
      ws <- DB.exec DB.getWebsites
      mapM_ pollWebsite ws

-- Poll a website
pollWebsite :: Website -> AppConfig IO ()
pollWebsite ws = do
      (b, _) <- poll ws
      when b $ do
            liftIO $ putStrLn "Changed"
            return ()

-- Poll all targets from the database
pollTargets :: AppConfig IO ()
pollTargets = do 
      ws <- DB.exec DB.getWebsites
      mapM_ (\w -> do
            (b, s) <- poll w
            when b $ do -- Website changed, continue checking targets
                  ts <- DB.exec $ DB.getTargetsOnWebsite (idWebsite w)
                  mapM_ (\t -> pollTarget t w s) ts) ws

-- Poll a target
pollTarget :: Target -> Website -> SiteContent -> AppConfig IO ()
pollTarget t w s = do
      let (Just e) = selector t
      let wid = idWebsite w
      liftIO $ putStrLn ("Polling: " ++ url w ++ " with target " ++ e) 
      let h = scrapeElement e s 
      b <- DB.exec $ DB.checkTargetHash wid h
      when b $ do
            _ <- DB.exec $ DB.updateTargetHash wid h
            liftIO $ putStrLn "Changed"
            return ()

-- Return site as string
getSite :: URL -> IO String
getSite u = getResponseBody =<< simpleHTTP (getRequest u)
