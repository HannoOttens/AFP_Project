module Handlers.Account (
    LoginAPI, accountServer 
) where

import Servant
import Debug.Trace

import DBAdapter
import Models.Register as RM
import Models.Login as LM
import PostRedirect


type LoginAPI = "login"    :> ReqBody '[FormUrlEncoded] LoginForm    :> PostRedirect 301 String
           :<|> "register" :> ReqBody '[FormUrlEncoded] RegisterForm :> PostRedirect 301 String


accountServer :: ServerT LoginAPI (AppM Handler)
accountServer = login 
           :<|> register 


register :: RegisterForm -> AppM Handler PostRedirectHandler
register form = trace "REGISTER" $ do
    return $ redirect "login.html"

login :: LoginForm -> AppM Handler PostRedirectHandler
login form = trace (LM.username form) $ do
    return $ redirect "account.html"
