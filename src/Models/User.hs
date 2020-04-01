{-|
Module      : Models.User
Description : User model

Database model for users.
-}
module Models.User where

import Data.Aeson
import Data.ByteString
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Servant.Auth.Server

-- | Type for user session
newtype Session = Session { userID :: Int } 
    deriving(Generic)

-- | User model for updating at retrieving users from the database
data User = User {
    id         :: Int,          
    username   :: String,
    password   :: ByteString
} deriving (Eq, Generic, Show)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToJSON Session
instance ToJWT Session
instance FromJSON Session
instance FromJWT Session