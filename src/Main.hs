import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Reader

import Handlers
import DBAdapter

type API = ReqBody '[PlainText] String :> Post '[JSON] NoContent
      :<|> Get '[JSON] [String]

server :: ServerT API (AppM Handler)
server = postMessage :<|> getMessages

api :: Proxy API
api = Proxy

runApp :: Config -> IO ()
runApp conf = run 8080 (serve api $ hoistServer api (flip runReaderT conf) server)

main :: IO ()
main = do
  let config = Config {dbFile = "db"}
  runReaderT initDB config
  runApp config