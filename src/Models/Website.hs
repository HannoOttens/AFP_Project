module Models.Website where

import Data.Time
import Database.SQLite.Simple

type URL  = String
type Hash = Int

-- | Specifies a website
data Website = Website {
    idWebsite  :: Int,      
    url        :: URL,     -- | URL of webiste
    lastUpdate :: Maybe UTCTime, -- | time of last change on website
    hash       :: Maybe Hash     -- | hash of website contents
}

instance FromRow Website where
    fromRow = Website <$> field <*> field <*> field <*> field
