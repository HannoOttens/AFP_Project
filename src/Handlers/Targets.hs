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
import Models.User as UM
import Config
import qualified DBAdapter as DB

type TargetID = Int
type TargetsAPI = "target" :> (
             "update" :> ReqBody '[JSON] FullTarget :> Get '[JSON] TargetID
        :<|> "delete" :> QueryParam "id" Int        :> Get '[JSON] Bool
        :<|> "list"                                 :> Get '[JSON] [FTM.FullTarget]
    )

targetServer :: ServerT TargetsAPI (AppContext Handler)
targetServer = editTarget :<|> deleteTarget :<|> listTargets

editTarget :: FullTarget -> AppContext Handler Int
editTarget ft = do
    user <- get
    trace (show user) $ return (FTM.targetID ft)

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
    targets <- DB.contextDbAction $ DB.getTargetsOfUser userId
    return targets
