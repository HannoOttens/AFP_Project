import Network.Wai.Handler.Warp
import Servant
import Control.Monad
import Control.Monad.Reader
import System.Cron.Schedule
import Data.Hashable

import DBAdapter
import Handlers.Account
import Config
import Scraper

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
pollWebsites = do h <- hash <$> scrapePage "http://www.cs.uu.nl/docs/vakken/afp/schedule.html"
                  b <- runReaderT (dbExec $ dbCheckWebsiteHash 0 h) config
                  when b $ do _ <- runReaderT (dbExec $ dbUpdateWebsiteHash 0 h) config
                              -- notify
                              return ()

main :: IO ()
main = do
  _ <- execSchedule $ do
        addJob pollWebsites $ pollSchedule config 
  runReaderT initDB config
  runApp config