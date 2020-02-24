import Network.Wai.Handler.Warp
import Servant

import Handlers
import DBAdapter

type API = ReqBody '[PlainText] String :> Post '[JSON] NoContent
      :<|> Get '[JSON] [String]

server :: FilePath -> Server API
server dbfile = postMessage dbfile 
           :<|> getMessages dbfile

api :: Proxy API
api = Proxy

runApp :: FilePath -> IO ()
runApp dbfile = run 8080 (serve api $ server dbfile)

main :: IO ()
main = do
  let dbfile = "db"
  initDB dbfile
  runApp dbfile