module DBAdapter where

import Database.SQLite.Simple
import Control.Monad.IO.Class
import Servant

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

dbExec :: FilePath -> (Connection -> IO a) -> Handler a
dbExec dbfile f = liftIO $ withConnection dbfile f

dbAddMessage :: String -> Connection -> IO ()
dbAddMessage msg conn = execute conn "INSERT INTO messages VALUES (?)" (Only msg)

dbGetMessages :: FromRow r => Connection -> IO [r]
dbGetMessages conn    = query_ conn  "SELECT msg FROM messages"