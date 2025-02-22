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

-- | The API type for notifications
type NotificationAPI = "notification" :> (
         "subscribe"     :> ReqBody '[FormUrlEncoded] SubscriptionDetails :> Post '[JSON] Response 
    :<|> "keys"                                                           :> Get  '[JSON] [Word8]
    :<|> "clients"                                                        :> Get  '[JSON] [SubscriptionDetails]
    :<|> "clientdelete"  :> QueryParam "token" String                     :> Get  '[JSON] Bool
    :<|> "list"                                                           :> Get  '[JSON] [Notification]
    :<|> "clearhistory"                                                   :> Get  '[JSON] Bool)

-- | The server instance for the NotificationAPI
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
    userId <- get
    result <- lift $ DB.liftDbAction $ DB.addToken userId details
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
    userId <- get
    DB.contextDbAction $ DB.getTokens userId

-- | Client a target from the users list of clients
deleteClient :: Maybe String -> AppContext Handler Bool
deleteClient tokenQ = trace "notification/clientdelete" $ do
    -- Check if query parameter is there
    unless (isJust tokenQ) (throwError err404)
    let token = fromJust tokenQ
    userId <- get
    -- Remove the target
    DB.contextDbAction $ DB.deleteToken userId token

-- | Get a list of all previous notifications
notifications :: AppContext Handler [Notification]
notifications = trace "notification/list" $ do
    userId <- get
    DB.contextDbAction $ DB.getNotificationHistory userId

-- | Clear the entire notification history
clearHistory :: AppContext Handler Bool
clearHistory = trace "notification/clearhistory" $ do
    userId <- get
    -- Remove the history
    DB.contextDbAction $ DB.deleteNotificationHistory userId
