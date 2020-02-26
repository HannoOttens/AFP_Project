module Handlers.Account (
    LoginAPI, accountServer 
) where

import Servant
import Debug.Trace

import DBAdapter
import Models.Register as RM
import Models.Login as LM


type LoginAPI = "login"    :> ReqBody '[FormUrlEncoded] LoginForm    :> Post '[FormUrlEncoded] NoContent
           :<|> "register" :> ReqBody '[FormUrlEncoded] RegisterForm :> Post '[FormUrlEncoded] NoContent


accountServer :: ServerT LoginAPI (AppM Handler)
accountServer = login 
           :<|> register 

register :: RM.RegisterForm -> AppM Handler NoContent
register form = trace "REGISTER" $ do
    return NoContent

login :: LM.LoginForm -> AppM Handler NoContent
login form = trace (LM.username form) $ do
    return NoContent
