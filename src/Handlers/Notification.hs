module Handlers.Notification where

import Servant
import Control.Monad.Reader
import Control.Monad.State
import Data.Word (Word8)
import Web.WebPush
import Debug.Trace


import Config
import Models.Notification
import qualified DBAdapter as DB 

type NotificationAPI = "subscribe" :> ReqBody '[FormUrlEncoded] SubscriptionDetails :> Post '[JSON] Response 
                  :<|> "keys" :> Get '[JSON] [Word8]

notificationServer :: ServerT NotificationAPI (AppContext Handler)
notificationServer = subscribe
                :<|> keys

-- | Store subscription details of user
subscribe :: SubscriptionDetails -> AppContext Handler Response
subscribe details = do user <- get
                       result <- lift $ DB.liftDbAction $ DB.addToken user details
                       trace "notification/subscribe" $ return $ Response result

-- | Get public key of server for notification subscription
keys :: AppContext Handler [Word8]
keys = do keyPair <- asks vapidKeys
          let publicKey = vapidPublicKeyBytes keyPair
          trace "notification/keys" $ return publicKey
