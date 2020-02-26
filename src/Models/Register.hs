module Models.Register where

import           GHC.Generics
import           Web.FormUrlEncoded          (FromForm)

-- | Used on the register page to register a user
data RegisterForm = RegisterForm
 { email   :: String
 , password :: String
 , rpassword :: String
 } deriving (Eq, Show, Generic)

-- | Instance to convert it out of form data
instance FromForm RegisterForm
