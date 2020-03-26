{-|
Module      : Handlers.Notification
Description : Notification handlers

Handlers for notification details.
-}
module Handlers.Notification where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Word (Word8)
import Debug.Trace
import Servant
import Web.WebPush

import qualified DBAdapter as DB 
import Config
import Models.Notification as MN
import Models.User as UM

type NotificationAPI = "notification" :> (
         "subscribe"     :> ReqBody '[FormUrlEncoded] SubscriptionDetails :> Post '[JSON] Response 
    :<|> "keys"                                                           :> Get  '[JSON] [Word8]
    :<|> "clients"                                                        :> Get  '[JSON] [SubscriptionDetails]
    :<|> "clientdelete"  :> QueryParam "token" String                     :> Get  '[JSON] Bool
    :<|> "list"                                                           :> Get  '[JSON] [Notification]
    :<|> "clearhistory"                                                   :> Get  '[JSON] Bool)

notificationServer :: ServerT NotificationAPI (AppContext Handler)
notificationServer = subscribe
                :<|> keys
                :<|> clients
                :<|> deleteClient
                :<|> notifications
                :<|> clearHistory

-- | Store subscription details of user
subscribe :: SubscriptionDetails -> AppContext Handler Response
subscribe details = trace "notification/subscribe" $ do 
    user <- get
    result <- lift $ DB.liftDbAction $ DB.addToken user details
    return $ Response result

-- | Get public key of server for notification subscription
keys :: AppContext Handler [Word8]
keys = trace "notification/keys" $ do 
    keyPair <- asks vapidKeys
    let publicKey = vapidPublicKeyBytes keyPair
    return publicKey

-- | Get public key of server for notification subscription
clients :: AppContext Handler [SubscriptionDetails]
clients = trace "notification/clients" $ do 
    userId <- gets UM.id
    DB.contextDbAction $ DB.getTokens userId

-- | Client a target from the users list of clients
deleteClient :: Maybe String -> AppContext Handler Bool
deleteClient tokenQ = trace "notification/clientdelete" $ do
    -- Check if query parameter is there
    unless (isJust tokenQ) (throwError err404)
    let token = fromJust tokenQ
    userId <- gets UM.id
    -- Remove the target
    DB.contextDbAction $ DB.deleteToken userId token


notifications :: AppContext Handler [Notification]
notifications = trace "notification/list" $ do
    userId <- gets UM.id
    DB.contextDbAction $ DB.getNotificationHistory userId

clearHistory :: AppContext Handler Bool
clearHistory = trace "notification/clearhistory" $ do
    userId <- gets UM.id
    -- Remove the history
    DB.contextDbAction $ DB.deleteNotificationHistory userId
