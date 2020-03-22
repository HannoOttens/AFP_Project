module Handlers.Notification where

import Servant
import Control.Monad.Reader
import Control.Monad.State
import Data.Word (Word8)
import Data.Maybe
import Web.WebPush
import Debug.Trace

import Config
import Models.Notification as MN
import Models.User as UM
import qualified DBAdapter as DB 

type NotificationAPI = "notification" :> (
         "subscribe"    :> ReqBody '[FormUrlEncoded] SubscriptionDetails :> Post '[JSON] Response 
    :<|> "keys"                                                          :> Get  '[JSON] [Word8]
    :<|> "clients"                                                       :> Get  '[JSON] [SubscriptionDetails]
    :<|> "clientdelete" :> QueryParam "token" String                     :> Get  '[JSON] Bool)

notificationServer :: ServerT NotificationAPI (AppContext Handler)
notificationServer = subscribe
                :<|> keys
                :<|> clients
                :<|> deleteClient

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
    clients <- DB.contextDbAction $ DB.getTokens userId
    return clients

-- | Client a target from the users list of clients
deleteClient :: Maybe String -> AppContext Handler Bool
deleteClient tokenQ = trace "notification/delete" $ do
    -- Check if query parameter is there
    unless (isJust tokenQ) (throwError err404)
    let token = fromJust tokenQ
    userId <- gets UM.id
    -- Remove the target
    DB.contextDbAction $ DB.deleteToken userId token