{-|
Module      : Models.Website
Description : Website model

Database model for websites.
-}
module Models.Website where

import Data.Time
import Database.SQLite.Simple

-- | Specifies a website
data Website = Website {
    idWebsite  :: Int,
    url        :: String, -- ^ URL of webiste      
    lastUpdate :: Maybe UTCTime, -- ^ Time of last change on website  
    hash       :: Maybe Int -- ^ Hash of website contents   
}

instance FromRow Website where
    fromRow = Website <$> field <*> field <*> field <*> field
