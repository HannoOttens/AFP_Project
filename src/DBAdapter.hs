{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module DBAdapter where
import Database.SQLite.Simple
import Control.Monad.IO.Class
import Control.Concurrent
import Network.Wai.Handler.Warp
import Servant
import Debug.Trace

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

dbExec :: FilePath -> (Connection -> IO a) -> Handler a
dbExec dbfile f = liftIO $ withConnection dbfile f


dbAddMessage msg conn = execute conn "INSERT INTO messages VALUES (?)" (Only msg)
dbGetMessages conn = query_ conn "SELECT msg FROM messages"