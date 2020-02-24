module Handlers where

import Servant
import Debug.Trace

import DBAdapter

postMessage :: String ->  FilePath ->  Handler NoContent
postMessage msg dbfile = trace "POST" $ do
    dbExec dbfile $ dbAddMessage msg
    return NoContent

getMessages :: FilePath -> Handler [String]
getMessages dbfile = trace "GET" $ dbExec dbfile dbGetMessages