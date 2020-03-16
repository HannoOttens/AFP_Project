module Handlers.Notification where

import Servant
import Control.Monad.Reader
import Control.Monad.State
import Data.Word (Word8)
import Web.WebPush


import Config
import Models.Notification
import Notification
import qualified DBAdapter as DB 

type NotificationAPI = "subscribe" :> ReqBody '[FormUrlEncoded] SubscriptionDetails :> Post '[JSON] Response 
                  :<|> "keys" :> Get '[JSON] [Word8]

notificationServer :: ServerT NotificationAPI (AppContext Handler)
notificationServer = subscribe
                :<|> keys

subscribe :: SubscriptionDetails -> AppContext Handler Response
subscribe details = do user <- get
                       result <- lift $ DB.liftDbAction $ DB.addToken user details
                       return $ Response result

keys :: AppContext Handler [Word8]
keys = do keyPair <- asks vapidKeys
          let publicKey = vapidPublicKeyBytes keyPair
          return publicKey
