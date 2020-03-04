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

config :: Config
config = Config {
      dbFile = "db",
      initFile = "tables.sqlite",
      pollSchedule = "0-59 * * * *"
}

type API = LoginAPI
      :<|> Raw

server :: ServerT API (AppM Handler)
server = accountServer
    :<|> serveDirectoryWebApp "www" 

api :: Proxy API
api = Proxy

runApp :: Config -> IO ()
runApp conf = run 8080 (serve api $ hoistServer api (`runReaderT` conf) server)

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

execDB :: (Connection -> IO a) -> IO a
execDB f = runReaderT (DB.exec f) config

main :: IO ()
main = do
  _ <- execSchedule $
        addJob (runReaderT pollWebsites config) $ pollSchedule config 
  runReaderT DB.initDB config
  runApp config