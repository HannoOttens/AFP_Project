module Models.Target where

import Database.SQLite.Simple

data Target = Target {
    id        :: Int,
    userID    :: Int,
    websiteID :: Int,
    selector  :: Maybe String  
}

instance FromRow Target where
    fromRow = Target <$> field <*> field <*> field <*> field

instance ToRow Target where
    toRow t = toRow (userID t, websiteID t, selector t)