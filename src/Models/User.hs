{-|
Module      : Models.User
Description : User model

Database model for users.
-}
module Models.User where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Servant.Auth.Server

data User = User {
    id         :: Int,          
    username   :: String,
    password   :: String
} deriving (Eq, Generic, Show)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User