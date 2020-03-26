module LoginTest where

import Data.String(fromString)
import Network.Wai (Application, Request(..))
import Network.Wai.Test
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Wai

import Models.Login
import Models.Notification
import Models.Register

import TestUtils

loginTests :: SpecWith (st, Application)
loginTests = describe "login and register endpoint" $ do
        it "responds with 301 after registering" $
            postForm "/register" registerForm `shouldRespondWith` 301
        it "responds with 200 after valid credentials" $ do
            postForm "/register" registerForm `shouldRespondWith` 301
            postForm "/login" loginFormValid `shouldRespondWithJson` (200, LoginResponse True "account.html")
        it "responds with 401 after invalid password" $ do
            postForm "/register" registerForm `shouldRespondWith` 301
            postForm "/login" loginFormInvalidPassword `shouldRespondWith` 401
        it "responds with 401 after invalid user" $ do
            postForm "/register" registerForm `shouldRespondWith` 301
            postForm "/login" loginFormInvalidUser `shouldRespondWith` 401


registerForm :: RegisterForm
registerForm = RegisterForm "test" "password" "password"

loginFormValid :: LoginForm
loginFormValid = LoginForm "test" "password"

loginFormInvalidPassword :: LoginForm
loginFormInvalidPassword = LoginForm "test" "false"

loginFormInvalidUser :: LoginForm
loginFormInvalidUser = LoginForm "test1" "password"