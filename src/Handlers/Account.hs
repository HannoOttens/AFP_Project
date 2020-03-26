{-|
Module      : Handlers.Account
Description : Account handlers

Handlers for account details.
-}
module Handlers.Account (
    LoginAPI, accountServer 
) where

import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Debug.Trace
import Servant
import Servant.Auth.Server

import Config
import DBAdapter as DB
import Models.Login as LM
import Models.Register as RM
import Models.User as UM
import PostRedirect

type LoginHeaders a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a
type LoginAPI = "login"    :> ReqBody '[FormUrlEncoded] LoginForm    :> Post '[JSON] (LoginHeaders LM.LoginResponse)
           :<|> "register" :> ReqBody '[FormUrlEncoded] RegisterForm :> PostRedirect 301 String

accountServer :: ServerT LoginAPI (AppConfig Handler)
accountServer = login 
           :<|> register 

-- | Register a user
register :: RegisterForm -> AppConfig Handler PostRedirectHandler
register form = trace "account/register" $
    if RM.password form == RM.rpassword form 
    then do 
        success <- liftDbAction $ DB.addUser UM.User { UM.id = 0,
                                                       UM.username = RM.username form, 
                                                       UM.password = RM.password form }
        case success of
          Just _  -> redirect "login.html"
          Nothing -> redirect "register.html"
    -- Failed because passwords are not equal
    else redirect "register.html"

-- | Log in a user
login :: LoginForm -> AppConfig Handler (LoginHeaders LM.LoginResponse)
login form = trace "account/login" $ do
    user <- liftDbAction $ DB.getUser (LM.username form)
    case user of
        Just u  -> if UM.password u == LM.password form
                   then returnLoginSuccess u
                   else throwError err401
        Nothing -> throwError err401


-- | Return a login succes with the correct Set-Cookie headers
returnLoginSuccess :: User -> AppConfig Handler (LoginHeaders LM.LoginResponse)
returnLoginSuccess user = do 
  conf <- ask
  mApplyCookies <- liftIO $ acceptLogin (cookieSettings conf) (jwtSettings conf) user
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> return . applyCookies $ LM.LoginResponse { loginSuccess = True, redirectTo = "account.html"}