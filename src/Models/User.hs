module Models.User where

import Database.SQLite.Simple
import Data.Aeson
import Servant.Auth.Server
import GHC.Generics (Generic)

data User = User {
    id         :: Int,          
    username   :: String,
    password   :: String
} deriving (Generic, Show)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User