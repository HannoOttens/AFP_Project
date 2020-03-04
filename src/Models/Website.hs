module Models.Website where

import Data.Time
import Database.SQLite.Simple

type URL = String
type Hash = String

-- | Specifies a website
data Website = Website {
    idWebsite  :: Int,      
    url        :: URL,     -- | URL of webiste
    lastUpdate :: UTCTime, -- | time of last change on website
    hash       :: Hash     -- | hash of website contents
}

instance FromRow Website where
    fromRow = Website <$> field <*> field <*> field <*> field