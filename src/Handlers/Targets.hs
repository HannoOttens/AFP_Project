module Handlers.Targets (
    TargetsAPI, targetServer 
) where

import Servant
import Debug.Trace
import Control.Monad.State


import Config

type TargetsAPI = "secure" :> Get '[JSON] NoContent

targetServer :: ServerT TargetsAPI (AppContext Handler)
targetServer = secure

secure :: AppContext Handler NoContent
secure = do
    user <- get
    trace (show user) $ return NoContent
                       
