{-|
Module      : Models.Target
Description : Target model

Database model for targets.
-}
module Models.Target where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics (Generic)

-- | Database model for saving and retrieving targets
data Target = Target {
    id        :: Int,
    userID    :: Int,
    websiteID :: Int,
    selector  :: Maybe String,  
    hash      :: Maybe Int     -- ^ hash of target content
} deriving (Generic, Show)

instance FromRow Target where
    fromRow = Target <$> field <*> field <*> field <*> field <*> field
instance ToRow Target where
    toRow t = toRow (userID t, websiteID t, selector t, hash t)
instance ToJSON Target
instance FromJSON Target