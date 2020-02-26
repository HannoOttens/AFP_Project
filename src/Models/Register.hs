module Models.Register where

import           GHC.Generics
import           Servant
import           Web.FormUrlEncoded          (FromForm)

data RegisterForm = RegisterForm
 { email   :: !T.Text
 , password :: !T.Text
 , rpassword :: !T.Text
 } deriving (Eq, Show, Generic)

instance FromForm RegisterForm
