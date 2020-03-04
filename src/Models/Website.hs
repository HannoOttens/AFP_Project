module Models.Website where

import Data.Time

type URL  = String
type Hash = Int

-- | Specifies a website
data Website = Website {
    idWebsite  :: Int,      
    url        :: URL,     -- | URL of webiste
    lastUpdate :: UTCTime, -- | time of last change on website
    hash       :: Hash     -- | hash of website contents
}