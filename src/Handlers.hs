module Handlers where

import Servant
import Debug.Trace
import Control.Monad.Reader

import DBAdapter

postMessage :: DB (String ->  Handler NoContent)
postMessage = trace "POST" $ do
    dbfile <- ask
    return (\msg -> do 
        _ <- runReader (dbExec $ dbAddMessage msg) dbfile
        return NoContent)

getMessages :: DB (Handler [String])
getMessages = trace "GET" $ dbExec dbGetMessages