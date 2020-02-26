module Models.User where

import Database.SQLite.Simple

data User = User {
    idUser     :: Int,          
    username   :: String,
    password   :: String
}

instance FromRow User where
    fromRow = User <$> field <*> field <*> field