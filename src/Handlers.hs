{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Handlers where
import Database.SQLite.Simple
import Control.Monad.IO.Class
import Control.Concurrent
import Network.Wai.Handler.Warp
import Servant
import Debug.Trace

import DBAdapter


postMessage :: String ->  FilePath ->  Handler NoContent
postMessage msg dbfile = trace "POST" $ do
    dbExec dbfile $ dbAddMessage msg
    return NoContent

getMessages :: FilePath -> Handler [String]
getMessages dbfile = trace "GET" $ fmap (map fromOnly) (dbExec dbfile dbGetMessages)