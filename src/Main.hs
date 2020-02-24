{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Database.SQLite.Simple
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp
import Servant
import Debug.Trace
import DBAdapter

type Message = String
type API = ReqBody '[PlainText] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]

api :: Proxy API
api = Proxy

server :: FilePath -> Server API
server dbfile = postMessage :<|> getMessages
  where 
        postMessage :: Message -> Handler NoContent
        postMessage msg = trace "POST" $ do
          dbExec dbfile $ dbAddMessage msg
          return NoContent

        getMessages :: Handler [Message]
        getMessages = trace "GET" $ fmap (map fromOnly) (dbExec dbfile dbGetMessages)

runApp :: FilePath -> IO ()
runApp dbfile = run 8080 (serve api $ server dbfile)


main :: IO ()
main = do
  let dbfile = "db"
  initDB dbfile
  runApp dbfile