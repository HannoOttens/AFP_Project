module Models.Login where

import GHC.Generics as T
import Servant 
import Web.FormUrlEncoded (FromForm)

data LoginForm = LoginForm
 { email   :: !T.Text
 , password :: !T.Text
 } deriving (Eq, Show, Generic)

instance FromForm LoginForm