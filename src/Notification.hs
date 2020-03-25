module Notification where

import Web.WebPush
import qualified Data.Text as T
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import qualified DBAdapter as DB
import Models.Notification
import Config

type PushMsg = PushNotification NotificationMessage

newPushNotification :: NotificationMessage -> SubscriptionDetails -> PushMsg
newPushNotification msg sub = set pushMessage msg notification
    where notification = mkPushNotification (endpoint sub) (hash sub) (auth sub)

-- | Lookup notifications details for user and construct push message 
createNotificationDetails :: Int -> NotificationMessage -> AppConfig IO [PushMsg]
createNotificationDetails userID msg = do 
    details <- DB.exec $ DB.getTokens userID
    return $ map (newPushNotification msg) details

-- | Send all notifications to recipients, if endpoint is not found return endpoint url so it can be removed from database
sendNotifications :: [PushMsg] -> AppConfig IO [String]
sendNotifications msgs = map (T.unpack . view pushEndpoint) <$> filterM p msgs
    where p msg = do keys <- asks vapidKeys
                     man <- asks manager
                     response <- sendPushNotification keys man msg
                     return $ case response of
                         Left RecepientEndpointNotFound -> True
                         _                              -> False