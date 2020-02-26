import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Reader

import Handlers
import DBAdapter

type API = ReqBody '[PlainText] String :> Post '[JSON] NoContent
      :<|> Get '[JSON] [String]

config :: Config
config = Config {
      dbFile = "db",
      initFile = "tables.sqlite"
}

server :: ServerT API (AppM Handler)
server = postMessage :<|> getMessages

api :: Proxy API
api = Proxy

runApp :: Config -> IO ()
runApp conf = run 8080 (serve api $ hoistServer api (`runReaderT` conf) server)

main :: IO ()
main = do
  runReaderT initDB config
  runApp config