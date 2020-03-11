module Models.Target where

import Database.SQLite.Simple

type Hash = Int

data Target = Target {
    websiteID :: Int,
    userID    :: Int,
    selector  :: Maybe String,
    hash      :: Maybe Hash     -- | hash of target content
}

instance FromRow Target where
    fromRow = Target <$> field <*> field <*> field <*> field