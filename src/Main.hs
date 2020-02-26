import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Reader

import Handlers
import DBAdapter

type API = ReqBody '[PlainText] String :> Post '[JSON] NoContent
      :<|> Get '[JSON] [String]

server :: DB (Server API)
server = liftM2 (:<|>) postMessage getMessages

api :: Proxy API
api = Proxy

runApp :: FilePath -> IO ()
runApp dbfile = run 8080 (serve api $ runReader server dbfile)

main :: IO ()
main = do
  let dbfile = "db"
  runReader initDB dbfile
  runApp dbfile