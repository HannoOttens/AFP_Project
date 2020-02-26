module Handlers.Account where

import Servant
import Debug.Trace

import DBAdapter
import Models.Register as RM
import Models.Login as LM


register :: RM.RegisterForm -> AppM Handler NoContent
register form = trace "REGISTER" $ do
    return NoContent

login :: LM.LoginForm -> AppM Handler NoContent
login form = trace "LOGIN" $ do
    return NoContent
