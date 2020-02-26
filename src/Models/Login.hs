module Models.Login where

import GHC.Generics
import Web.FormUrlEncoded (FromForm)

-- | Used for logging in
data LoginForm = LoginForm
 { email   :: String
 , password :: String
 } deriving (Eq, Show, Generic)

-- | Instance to convert formdata to the `LoginForm` model
instance FromForm LoginForm