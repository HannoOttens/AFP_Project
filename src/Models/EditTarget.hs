module Models.EditTarget where

import Data.Aeson
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm)

-- | Specifies what gets returned from an edit operation
data EditTarget = FullTarget {
    targetID    :: Int,
    websiteUrl  :: String,
    selector    :: Maybe String 
} deriving (Generic, Show)

instance ToJSON EditTarget
instance FromJSON EditTarget
instance FromForm EditTarget
