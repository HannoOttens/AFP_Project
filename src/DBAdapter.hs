{-# LANGUAGE OverloadedStrings #-}

module DBAdapter where

import Database.SQLite.Simple
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader(ReaderT, asks, ask)
import Servant
import Data.String

-- | Configuration 
data Config = Config
  { 
    dbFile   :: String, -- | Path to the sqlite database file
    initFile :: String  -- | Path to the file containing create table statement
  }

type AppM a = ReaderT Config a

-- | Initialize database with tables if do not already exist
initDB :: AppM IO ()
initDB = do
  config <- ask
  liftIO $ do 
    initQuery <- readFile (initFile config)
    withConnection (dbFile config) (`execute_` fromString initQuery)

-- | Execute an action on the database
dbExec :: (Connection -> IO a) -> AppM Handler a
dbExec f = do 
  file <- asks dbFile
  liftIO $ withConnection file f

dbAddMessage :: String -> Connection -> IO ()
dbAddMessage msg conn = execute conn "INSERT INTO messages VALUES (?)" (Only msg)

dbGetMessages :: Connection -> IO [String]
dbGetMessages conn = let result = query_ conn "SELECT msg FROM messages"
                     in fmap (map fromOnly) result