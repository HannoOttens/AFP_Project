module Models.FullTarget where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics (Generic)

-- | Specifies a website plus target
data FullTarget = FullTarget {
    targetID    :: Int,
    userID      :: Int,
    websiteID   :: Int,
    websiteUrl  :: String,
    lastUpdate  :: String,
    selector    :: Maybe String 
} deriving (Generic, Show)

instance FromRow FullTarget where
    fromRow = FullTarget <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON FullTarget
instance FromJSON FullTarget

