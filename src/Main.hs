
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Control.Monad
import Control.Monad.Reader
import System.Cron.Schedule
import qualified Data.Hashable as H
import Debug.Trace

import DBAdapter as DB
import Handlers.Account
import Handlers.Targets
import Config
import Scraper
import Models.Website
import Models.User

type PublicAPI = LoginAPI 
type ProtectedAPI = TargetsAPI
type RawFiles = Raw
type API = PublicAPI  
            :<|> Servant.Auth.Server.Auth '[Cookie] User :> ProtectedAPI
            :<|> RawFiles

rawFiles :: ServerT RawFiles (AppM Handler)
rawFiles = serveDirectoryWebApp "www"

protected :: Servant.Auth.Server.AuthResult User -> ServerT ProtectedAPI (AppM Handler)
protected (Servant.Auth.Server.Authenticated user) = local (\cfg -> cfg { currentUser = Just user }) targetServer
protected _ = throwAll err401

public:: ServerT PublicAPI (AppM Handler)
public = accountServer

server :: ServerT API (AppM Handler)
server = public 
      :<|> protected 
      :<|> rawFiles

api :: Proxy API 
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