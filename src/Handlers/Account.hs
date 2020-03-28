{-|
Module      : Handlers.Account
Description : Account handlers

Handlers for account details.
-}
module Handlers.Account (
    LoginAPI, accountServer 
) where

import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans (liftIO)
import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.ByteString.Char8 (ByteString, pack)
import Debug.Trace
import Servant
import Servant.Auth.Server

import Config
import DBAdapter as DB
import Models.Login as LM
import Models.Register as RM
import Models.User as UM
import PostRedirect

-- | Type synonym for set-cookie headers
type LoginHeaders a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a
-- | The account API
type LoginAPI = "login"    :> ReqBody '[FormUrlEncoded] LoginForm    :> Post '[JSON] (LoginHeaders LM.LoginResponse)
           :<|> "register" :> ReqBody '[FormUrlEncoded] RegisterForm :> PostRedirect 301 String

-- | The server instance for the LoginAPI
accountServer :: ServerT LoginAPI (AppConfig Handler)
accountServer = login 
           :<|> register 

-- | Register a user
register :: RegisterForm -> AppConfig Handler PostRedirectHandler
register form = trace "account/register" $
    if RM.password form == RM.rpassword form 
    then do
        hashedPassword <- hashPassword (RM.password form)
        success <- liftDbAction $ DB.addUser UM.User { UM.id = 0,
                                                       UM.username = RM.username form, 
                                                       UM.password = hashedPassword }
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
        Just u  -> if verifyUser (LM.password form) u
                   then returnLoginSuccess u
                   else throwError err401
        Nothing -> throwError err401


-- | Return a login success with the correct Set-Cookie headers
returnLoginSuccess :: User -> AppConfig Handler (LoginHeaders LM.LoginResponse)
returnLoginSuccess user = do 
  conf <- ask
  mApplyCookies <- liftIO $ acceptLogin (cookieSettings conf) (jwtSettings conf) (Session $ UM.id user)
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> return . applyCookies $ LM.LoginResponse { loginSuccess = True, redirectTo = "account.html"}


hashPassword :: String -> AppConfig Handler ByteString
hashPassword pass = do 
    strength <- asks passwordStrength
    liftIO $ makePassword (pack pass) strength

verifyUser :: String -> User -> Bool
verifyUser attempt user = verifyPassword (pack attempt) (UM.password user)