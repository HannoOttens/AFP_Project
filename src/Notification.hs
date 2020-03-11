module Notification where

import Web.WebPush
import qualified Data.Text as T
import Control.Lens
import Control.Monad
import Network.HTTP.Client

import qualified DBAdapter as DB
import Models.Notification
import Config

type PushMsg = PushNotification NotificationMessage

newPushNotification :: NotificationMessage -> (String, String, String) -> PushMsg
newPushNotification msg (endpoint, hash, auth) = set pushMessage msg notification
    where notification = mkPushNotification (T.pack endpoint) (T.pack hash) (T.pack auth)

-- | Lookup notifications details for user and construct push message 
createNotificationDetails :: Int -> NotificationMessage -> AppConfig IO [PushMsg]
createNotificationDetails userID msg = do 
    details <- DB.exec $ DB.getTokens userID
    return $ map (newPushNotification msg) details

-- | Send all notifications to recipients, if endpoint is not found return endpoint url so it can be removed from database
sendNotifications :: VAPIDKeys -> Manager -> [PushMsg] -> AppConfig IO [String]
sendNotifications keys m msgs = map (T.unpack . view pushEndpoint) <$> filterM p msgs
    where p msg = do response <- sendPushNotification keys m msg
                     return $ case response of
                         Left RecepientEndpointNotFound -> True
                         _                              -> False