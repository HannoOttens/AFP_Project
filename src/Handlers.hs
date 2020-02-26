module Handlers where

import Servant
import Debug.Trace
import Control.Monad.Reader

import DBAdapter

postMessage :: String -> AppM Handler NoContent
postMessage msg = trace "POST" $ do
    Config {dbFile = f} <- ask
    dbExec $ dbAddMessage msg
    return NoContent

getMessages :: AppM Handler [String]
getMessages = trace "GET" $ dbExec dbGetMessages