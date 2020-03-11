module Models.Target where

import Database.SQLite.Simple

type Hash = Int

data Target = Target {
    id        :: Int,
    userID    :: Int,
    websiteID :: Int,
    selector  :: Maybe String  
    hash      :: Maybe Hash     -- | hash of target content
}

instance FromRow Target where
    fromRow = Target <$> field <*> field <*> field <*> field <*> field

instance ToRow Target where
    toRow t = toRow (userID t, websiteID t, selector t, hash t)