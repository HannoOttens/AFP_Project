{-|
Module      : Models.Login
Description : Login model

Database model for login.
-}
module Models.Login where

import Data.Aeson
import GHC.Generics
import Web.FormUrlEncoded (FromForm)

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
