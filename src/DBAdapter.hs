module DBAdapter where

import Database.SQLite.Simple
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader(ReaderT, ask, asks)
import Servant

--type DB a = Reader String a

data Config = Config
  { 
    dbFile :: String
  }

type AppM a = ReaderT Config a

initDB :: AppM IO ()
initDB = do
  file <- asks dbFile
  liftIO $ withConnection file (\conn ->
    execute_ conn
      "CREATE TABLE IF NOT EXISTS messages (msg text not null)")

dbExec :: (Connection -> IO a) -> AppM Handler a
dbExec f = do 
  Config {dbFile = file} <- ask
  liftIO $ withConnection file f

dbAddMessage :: String -> Connection -> IO ()
dbAddMessage msg conn = execute conn "INSERT INTO messages VALUES (?)" (Only msg)

dbGetMessages :: Connection -> IO [String]
dbGetMessages conn  = let result = query_ conn  "SELECT msg FROM messages"
                      in fmap (map fromOnly) result