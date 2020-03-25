module Handlers.Targets (
    TargetsAPI, targetServer 
) where

import Servant
import Debug.Trace
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State

import Models.Target as TM
import Models.FullTarget as FTM
import Models.EditTarget as ETM
import Models.Website as WM
import Models.User as UM
import Config
import qualified DBAdapter as DB

type TargetsAPI = "target" :> (
             "delete" :> QueryParam "id" Int                  :> Get '[JSON] Bool
        :<|> "update" :> ReqBody '[FormUrlEncoded] EditTarget :> Post '[JSON] Bool
        :<|> "list"                                           :> Get '[JSON] [FTM.FullTarget]
    )

targetServer :: ServerT TargetsAPI (AppContext Handler)
targetServer =  deleteTarget :<|> editTarget :<|> listTargets

editTarget :: ETM.EditTarget -> AppContext Handler Bool
editTarget targetModel = trace "target/update" $ do
    -- Only allow users to edit their own targets
    userId <- gets UM.id
    existingTarget <- DB.contextDbAction $ DB.getTarget (ETM.targetID targetModel)
    unless (isNothing existingTarget || userId == TM.userID (fromJust existingTarget)) (throwError err401)
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
                           , TM.hash = Nothing }
    -- Insert or update depending on the posted ID
    case ETM.targetID targetModel of
        0 -> DB.contextDbAction $ DB.addTarget target
        _ -> DB.contextDbAction $ DB.editTarget target

deleteTarget :: Maybe Int -> AppContext Handler Bool
deleteTarget targetIdQ = trace "target/delete" $ do
    -- Check if query parameter is there
    unless (isJust targetIdQ) (throwError err404)
    let targetId = fromJust targetIdQ
    userId <- gets UM.id
    target <- DB.contextDbAction $ DB.getTarget targetId
    -- The target might not exists anymore
    unless (isJust target) (throwError err500)
    -- Only allow a user to delete his own targets
    let targetUserId = TM.userID $ fromJust target
    unless (targetUserId == userId) (throwError err401)
    -- Remove the target
    DB.contextDbAction $ DB.removeTarget targetId

listTargets ::  AppContext Handler [FTM.FullTarget]
listTargets = trace "target/list" $ do
    userId <- gets UM.id
    DB.contextDbAction $ DB.getTargetsOfUser userId