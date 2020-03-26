{-|
Module      : Models.FullTarget
Description : Full target model

Database model for full targets.
-}
module Models.FullTarget where

import Data.Aeson
import Data.Time (UTCTime)
import Database.SQLite.Simple
import GHC.Generics (Generic)

-- | Specifies a website plus target
data FullTarget = FullTarget {
    targetID    :: Int,
    userID      :: Int,
    websiteID   :: Int,
    websiteUrl  :: String,
    lastUpdate  :: UTCTime,
    selector    :: Maybe String 
} deriving (Generic, Show)

instance FromRow FullTarget where
    fromRow = FullTarget <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON FullTarget
instance FromJSON FullTarget

