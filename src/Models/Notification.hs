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


data Notification = Notification { 
    website   :: Text, 
    message   :: Text, 
    timestamp :: Int 
} deriving (Generic)

instance A.ToJSON Notification
instance FromRow Notification where
    fromRow = Notification <$> field <*> field <*> field


data SubscriptionDetails = SubscriptionDetails {
    endpoint :: Text,
    hash     :: Text,
    auth     :: Text,
    device   :: Text,
    browser  :: Text
} deriving (Generic)

instance FromForm SubscriptionDetails
instance A.ToJSON SubscriptionDetails
instance FromRow SubscriptionDetails where
    fromRow = SubscriptionDetails <$> field <*> field <*> field <*> field <*> field

data Response = Response { 
    success :: Bool 
 } deriving (Eq, Show, Generic)

instance A.ToJSON Response  

newNotification :: String -> String -> NotificationMessage
newNotification t b = NotificationMessage {
    title = pack t,
    body = pack b,
    icon = "icon.png",
    vibrate = [100,50,100]
}