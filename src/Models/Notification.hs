module Models.Notification where

import qualified Data.Aeson as A
import GHC.Generics
import Data.Text

data NotificationMessage = NotificationMessage { 
    title   :: Text, 
    body    :: Text, 
    icon    :: Text, 
    vibrate :: [Int]
} deriving (Eq, Show, Generic)

instance A.ToJSON NotificationMessage

newNotification :: String -> String -> NotificationMessage
newNotification t b = NotificationMessage {
    title = pack t,
    body = pack b,
    icon = "icon.png",
    vibrate = [100,50,100]
}