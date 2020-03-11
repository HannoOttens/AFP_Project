module Handlers.Targets (
    TargetsAPI, targetServer 
) where

import Servant
import Debug.Trace
import Control.Monad.Reader

import Config

type TargetsAPI = "secure" :> Get '[JSON] NoContent

targetServer :: ServerT TargetsAPI (AppM Handler)
targetServer = secure


secure :: AppM Handler NoContent
secure = do
    ~(Just user) <- asks currentUser
    trace (show user) $ return NoContent

-- | Delete a target
-- deleteTarget :: Int -> AppM Handler PostRedirectHandler

-- | Update a target

-- | Update a target
