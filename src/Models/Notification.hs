module Models.Notification where

import qualified Data.Aeson as A
import GHC.Generics
import Data.Text
import Database.SQLite.Simple
import Web.FormUrlEncoded (FromForm)

data NotificationMessage = NotificationMessage { 
    title   :: Text, 
    body    :: Text, 
    icon    :: Text, 
    vibrate :: [Int]
} deriving (Generic)

instance A.ToJSON NotificationMessage

data SubscriptionDetails = SubscriptionDetails {
    endpoint :: Text,
    hash     :: Text,
    auth     :: Text
} deriving (Generic)

instance FromForm SubscriptionDetails

data Response = Response { 
    success :: Bool 
 } deriving (Eq, Show, Generic)

instance A.ToJSON Response  

instance FromRow SubscriptionDetails where
    fromRow = SubscriptionDetails <$> field <*> field <*> field

newNotification :: String -> String -> NotificationMessage
newNotification t b = NotificationMessage {
    title = pack t,
    body = pack b,
    icon = "icon.png",
    vibrate = [100,50,100]
}