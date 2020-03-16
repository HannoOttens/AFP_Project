
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import System.Cron.Schedule
import qualified Data.Hashable as H

import qualified DBAdapter as DB
import Handlers.Account
import Handlers.Targets
import Config
import Scraper
import Models.Website
import Models.User
import Models.Target

type PublicAPI = LoginAPI 
type ProtectedAPI = TargetsAPI
type RawFiles = Raw
type API = PublicAPI  
            :<|> Servant.Auth.Server.Auth '[Cookie] User :> (TargetsAPI)
            :<|> RawFiles

rawFiles :: ServerT RawFiles (AppConfig Handler)
rawFiles = serveDirectoryWebApp "www"

protected :: Servant.Auth.Server.AuthResult User -> ServerT ProtectedAPI (AppConfig Handler)
protected (Servant.Auth.Server.Authenticated user) = 
      let protectedAPI = (Proxy :: Proxy ProtectedAPI)      
      in hoistServer protectedAPI (`evalStateT` user) targetServer 
protected _ = throwAll err401

public:: ServerT PublicAPI (AppConfig Handler)
public = accountServer

server :: ServerT API (AppConfig Handler)
server = public 
      :<|> protected 
      :<|> rawFiles

api :: Proxy API 
api = Proxy

runApp :: Config -> IO ()
runApp conf = do
      let authApi = (Proxy :: Proxy '[CookieSettings, JWTSettings])
      run 8080 (serveWithContext api (authConf conf) $ hoistServerWithContext api authApi (`runReaderT` conf) server)

pollWebsites :: AppConfig IO ()
pollWebsites = do 
      ws <- DB.exec DB.getWebsites
      mapM_ pollWebsite ws

pollWebsite :: Website -> AppConfig IO ()
pollWebsite ws = do
      let u   = url ws
      let wid = idWebsite ws
      liftIO $ putStrLn ("Polling: " ++ u)
      h <- liftIO $ H.hash <$> scrapePage u
      b <- DB.exec $ DB.checkWebsiteHash wid h
      when b $ do _ <- DB.exec $ DB.updateWebsiteHash wid h
                  liftIO $ putStrLn "Changed"
                  return ()

pollTargets :: AppConfig IO ()
pollTargets = do 
      ws <- DB.exec DB.getWebsites
      mapM_ (\w -> do
            let u   = url w
            let wid = idWebsite w
            ts <- DB.exec $ DB.getTargetsOnWebsite wid
            liftIO $ putStrLn ("Polling: " ++ u ++ " with targets")
            h <- liftIO $ H.hash <$> scrapePage u        
            b <- DB.exec $ DB.checkWebsiteHash wid h
            when b $ do
                  _ <- DB.exec $ DB.updateWebsiteHash wid h
                  mapM_ (`pollTarget` w) ts) ws
      
pollTarget :: Target -> Website -> AppConfig IO ()
pollTarget t w = do
      let (Just e) = selector t
      let u        = url w
      let wid      = idWebsite w
      liftIO $ putStrLn ("Polling: " ++ u ++ " with target " ++ e)
      h <- liftIO $ H.hash <$> scrapeElement e u
      b <- DB.exec $ DB.checkTargetHash wid h
      when b $ do
            _ <- DB.exec $ DB.updateTargetHash wid h
            liftIO $ putStrLn "Changed"
            return ()

main :: IO ()
main = do
  conf <- config
  _ <- execSchedule $
        addJob (runReaderT pollWebsites conf) $ pollSchedule conf
  _ <- execSchedule $
        addJob (runReaderT pollTargets  conf) $ pollSchedule conf
  runReaderT DB.initDB conf
  runApp conf