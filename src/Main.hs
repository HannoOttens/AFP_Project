{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Network.Wai.Handler.Warp
import Servant
import Debug.Trace

type Message = String

type API = ReqBody '[PlainText] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]

api :: Proxy API
api = Proxy

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

server :: FilePath -> Server API
server dbfile = postMessage :<|> getMessages

  where 
        postMessage :: Message -> Handler NoContent
        postMessage msg = trace "POST" $ do
          liftIO . withConnection dbfile $ \conn ->
            execute conn
                    "INSERT INTO messages VALUES ('?')"
                    (Only msg)
          return NoContent

        getMessages :: Handler [Message]
        getMessages = trace "GET" $ fmap (map fromOnly) . liftIO $
         withConnection dbfile $ \conn ->
            query_ conn "SELECT msg FROM messages"

runApp :: FilePath -> IO ()
runApp dbfile = run 8080 (serve api $ server dbfile)


main :: IO ()
main = do
  let dbfile = "db"
  initDB dbfile
  runApp dbfile