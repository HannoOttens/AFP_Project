module Handlers.Targets (
    TargetsAPI, targetServer 
) where

import Servant
import Debug.Trace
import Servant.Auth.Server
import Control.Monad.Reader

import Models.User as UM
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
