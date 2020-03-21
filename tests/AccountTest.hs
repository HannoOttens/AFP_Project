module AccountTest where

import Network.Wai (Application)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Wai(shouldRespondWith)
import Data.String(fromString)
import Web.FormUrlEncoded(ToForm)

import Models.Register
import Models.Login

import TestUtils

accountTests :: SpecWith (st, Application)
accountTests = describe "register user" $ do
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

instance ToForm RegisterForm
instance ToForm LoginForm