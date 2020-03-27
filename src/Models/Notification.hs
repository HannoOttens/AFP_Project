{-|
Module      : Models.Notification
Description : Notification model

Database model for notifications.
-}
module Models.Notification where

import Data.Aeson
import Data.Text
import Data.Time
import Database.SQLite.Simple
import GHC.Generics
import Web.FormUrlEncoded (FromForm)

-- | A notification as to be sent to all clients
data NotificationMessage = NotificationMessage { 
    title   :: Text, 
    body    :: Text,
    url     :: Text, 
    icon    :: Text
} deriving (Generic)

instance ToJSON NotificationMessage

-- | A notification for display in the notification history
data Notification = Notification { 
    website   :: Text, 
    message   :: Text, 
    timestamp :: UTCTime 
} deriving (Generic)

instance ToJSON Notification
instance FromRow Notification where
    fromRow = Notification <$> field <*> field <*> field

-- | All details of a notification subscription
data SubscriptionDetails = SubscriptionDetails {
    endpoint :: Text,
    hash     :: Text,
    auth     :: Text,
    device   :: Text,
    browser  :: Text
} deriving (Generic)

instance FromForm SubscriptionDetails
instance ToJSON SubscriptionDetails
instance FromRow SubscriptionDetails where
    fromRow = SubscriptionDetails <$> field <*> field <*> field <*> field <*> field

-- | True / false response for boolean operations
newtype Response = Response { 
    success :: Bool 
 } deriving (Eq, Show, Generic)

instance ToJSON Response  

-- | Create a new notification
newNotification :: String -> String -> String -> NotificationMessage
newNotification t b u = NotificationMessage {
    title = pack t,
    body = pack b,
    url = pack u,
    icon = "favicon.ico"
}