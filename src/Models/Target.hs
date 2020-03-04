module Models.Target where

import Database.SQLite.Simple

data Target = Target {
    websiteID :: Int,
    userID    :: Int,
    selector  :: Maybe String  
}

instance FromRow Target where
    fromRow = Target <$> field <*> field <*> field