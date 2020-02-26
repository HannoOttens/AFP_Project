module DBAdapter where

import Database.SQLite.Simple
import Control.Monad.IO.Class
import Control.Monad.Reader
import Servant

type DB a = Reader String a

initDB :: DB (IO ())
initDB = do 
  dbfile <- ask
  return $ withConnection dbfile $ \conn ->
    execute_ conn
      "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

dbExec :: (Connection -> IO a) -> DB (Handler a)
dbExec f = do 
  dbfile <- ask
  return $ liftIO $ withConnection dbfile f

dbAddMessage :: String -> Connection -> IO ()
dbAddMessage msg conn = execute conn "INSERT INTO messages VALUES (?)" (Only msg)

dbGetMessages :: Connection -> IO [String]
dbGetMessages conn  = let result = query_ conn  "SELECT msg FROM messages"
                      in fmap (map fromOnly) result