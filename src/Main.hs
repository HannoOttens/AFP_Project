import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Reader

import DBAdapter
import Models.Login
import Models.Register

import Handlers.Account

type API = LoginAPI

type LoginAPI = ReqBody '[FormUrlEncoded] LoginForm :> Post '[JSON] NoContent
           :<|> ReqBody '[FormUrlEncoded] RegisterForm :> Post '[JSON] NoContent

server :: ServerT API (AppM Handler)
server = login :<|> register 

api :: Proxy API
api = Proxy

runApp :: Config -> IO ()
runApp conf = run 8080 (serve api $ hoistServer api (flip runReaderT conf) server)

main :: IO ()
main = do
  let config = Config {dbFile = "db"}
  runReaderT initDB config
  runApp config