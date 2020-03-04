module Handlers.Account (
    LoginAPI, accountServer 
) where

import Servant
import Debug.Trace

import DBAdapter as DB
import Models.Register as RM
import Models.Login as LM
import Models.User as UM
import PostRedirect
import Config

type TargetAPI = "delete" :> ReqBody '[FormUrlEncoded] LoginForm    :> PostRedirect 301 String

accountServer :: ServerT LoginAPI (AppM Handler)
accountServer = login 
           :<|> register 

-- | Delete a target
deleteTarget :: Int -> AppM Handler PostRedirectHandler

-- | Update a target

-- | Update a target
