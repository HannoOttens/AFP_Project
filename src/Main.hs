import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Reader
import System.Cron.Schedule

import DBAdapter
import Handlers.Account


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
pollWebsites = putStrLn "HeY! A MaN Has FaLLeN iN tHe WaTER in LEgo CiTY!!!"

main :: IO ()
main = do
  pids <- execSchedule $ do
        addJob poll $ pollSchedule config 
  runReaderT initDB config
  runApp config