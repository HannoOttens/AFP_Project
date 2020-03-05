module Handlers.Account (
    LoginAPI, accountServer 
) where

import Servant
import Debug.Trace
import Control.Monad.Trans.Reader(ask)

import DBAdapter as DB
import Models.Register as RM
import Models.Login as LM
import Models.User as UM
import PostRedirect
import Config

type LoginAPI = "login"    :> ReqBody '[FormUrlEncoded] LoginForm    :> PostCookieRedirect 301 String
           :<|> "register" :> ReqBody '[FormUrlEncoded] RegisterForm :> PostRedirect 301 String

accountServer :: ServerT LoginAPI (AppM Handler)
accountServer = login 
           :<|> register 

-- | Register a user
register :: RegisterForm -> AppM Handler PostRedirectHandler
register form = trace "account/register" $ do
    if RM.password form == RM.rpassword form
    then do
        success <- liftDbAction $ DB.addUser UM.User { UM.id       = 0,
                                                UM.username = RM.username form, 
                                                UM.password = RM.password form }
        case success of
         Just _  -> redirect "login.html"
         Nothing -> redirect "register.html"
    -- Failed because passwords are not equal
    else redirect "register.html"

-- | Log in a user
login :: LoginForm -> AppM Handler LoginHandler
login form = trace "account/login" $ do
    user <- liftDbAction $ DB.getUser (LM.username form)
    conf <- ask
    case user of
        Just u  -> if UM.password u == LM.password form
                   then redirectWithCookie conf u "account.html"
                   else throwError err401
        Nothing -> throwError err401
