module Handlers where

import Servant
import Debug.Trace

import DBAdapter

postMessage :: String -> AppM Handler NoContent
postMessage msg = trace "POST" $ do
    dbExec $ dbAddMessage msg
    return NoContent

getMessages :: AppM Handler [String]
getMessages = trace "GET" $ dbExec dbGetMessages