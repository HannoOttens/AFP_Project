import Network.Wai.Handler.Warp
import Database.SQLite.Simple
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

execDB :: (Connection -> IO a) -> IO a
execDB f = runReaderT (DB.exec f) config

main :: IO ()
main = do
  _ <- execSchedule $ do
        addJob pollWebsites $ pollSchedule config 
  runReaderT initDB config
  runApp config