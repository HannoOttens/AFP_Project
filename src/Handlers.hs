module Handlers where

import Database.SQLite.Simple
import Servant
import Debug.Trace

import DBAdapter

postMessage :: String ->  FilePath ->  Handler NoContent
postMessage msg dbfile = trace "POST" $ do
    dbExec dbfile $ dbAddMessage msg
    return NoContent

getMessages :: FilePath -> Handler [String]
getMessages dbfile = trace "GET" $ fmap (map fromOnly) (dbExec dbfile dbGetMessages)