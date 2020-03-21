module Models.Login where

import GHC.Generics
import Web.FormUrlEncoded (FromForm)
import Data.Aeson

-- | Used for logging in
data LoginForm = LoginForm
 { username :: String
 , password :: String
 } deriving (Eq, Show, Generic)
instance FromForm LoginForm

-- | Response after logging in
data LoginResponse = LoginResponse 
 { loginSuccess :: Bool
 , redirectTo :: String   
 } deriving (Eq, Show, Generic)
instance ToJSON LoginResponse  
