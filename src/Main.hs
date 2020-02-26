import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Reader

import DBAdapter
import Handlers.Account

type API = LoginAPI
      :<|> Raw
        
config :: Config
config = Config {
      dbFile = "db",
      initFile = "tables.sqlite"
}

server :: ServerT API (AppM Handler)
server = accountServer
    :<|> serveDirectoryWebApp "www" 

api :: Proxy API
api = Proxy

runApp :: Config -> IO ()
runApp conf = run 8080 (serve api $ hoistServer api (`runReaderT` conf) server)

main :: IO ()
main = do
  runReaderT initDB config
  runApp config