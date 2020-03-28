{-|
Module      : Handlers.Targets
Description : Targets handlers

Handlers for targets details.
-}
module Handlers.Targets (
    TargetsAPI, targetServer 
) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import Network.URI (isURI)
import Servant

import qualified DBAdapter as DB
import Config
import Models.EditTarget as ETM
import Models.FullTarget as FTM
import Models.Target as TM
import Models.Website as WM

-- | API for the target area, all listed under 'target/'
type TargetsAPI = "target" :> (
             "delete" :> QueryParam "id" Int                  :> Get '[JSON] Bool
        :<|> "update" :> ReqBody '[FormUrlEncoded] EditTarget :> Post '[JSON] Bool
        :<|> "list"                                           :> Get '[JSON] [FTM.FullTarget]
    )

-- | The server instance for the TargetsAPI
targetServer :: ServerT TargetsAPI (AppContext Handler)
targetServer =  deleteTarget :<|> editTarget :<|> listTargets

-- | Change or create a target
editTarget :: ETM.EditTarget -> AppContext Handler Bool
editTarget targetModel = trace "target/update" $ do
    -- Only allow users to edit their own targets
    userId <- get
    existingTarget <- DB.contextDbAction $ DB.getTarget (ETM.targetID targetModel)
    unless (isNothing existingTarget || userId == TM.userID (fromJust existingTarget)) (throwError err401)
    -- Check if target website is valid url
    unless (isURI $ ETM.websiteUrl targetModel) (throwError err500)
    -- Insert the website (if it does not exist) and get the ID
    let website = WM.Website { WM.url = ETM.websiteUrl targetModel
                             , WM.idWebsite = 0
                             , WM.lastUpdate = Nothing
                             , WM.hash = Nothing }
    websiteId <- DB.contextDbAction $ DB.addWebsite website
    -- Make the target
    let target = TM.Target { TM.id = ETM.targetID targetModel
                           , TM.userID = userId
                           , TM.websiteID = websiteId
                           , TM.selector = ETM.selector targetModel
                           , TM.hash = Nothing
                           , TM.content = "" }
    -- Insert or update depending on the posted ID
    case ETM.targetID targetModel of
        0 -> DB.contextDbAction $ DB.addTarget target
        _ -> DB.contextDbAction $ DB.editTarget target

-- | Remove a target from the database
deleteTarget :: Maybe Int -> AppContext Handler Bool
deleteTarget targetIdQ = trace "target/delete" $ do
    -- Check if query parameter is there
    unless (isJust targetIdQ) (throwError err404)
    let targetId = fromJust targetIdQ
    userId <- get
    target <- DB.contextDbAction $ DB.getTarget targetId
    -- The target might not exists anymore
    unless (isJust target) (throwError err500)
    -- Only allow a user to delete his own targets
    let targetUserId = TM.userID $ fromJust target
    unless (targetUserId == userId) (throwError err401)
    -- Remove the target
    DB.contextDbAction $ DB.removeTarget targetId

-- | List all target of the current user
listTargets ::  AppContext Handler [FTM.FullTarget]
listTargets = trace "target/list" $ do
    userId <- get
    DB.contextDbAction $ DB.getTargetsOfUser userId