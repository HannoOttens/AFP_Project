module NotificationTest where

import Data.String (fromString)
import Network.Wai (Application, Request(..))
import Network.Wai.Test
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Wai

import Models.Login
import Models.Notification
import Models.Register

import TestUtils

notificationTests :: SpecWith (st, Application)
notificationTests = describe "notification endpoint" $ do
        describe "/keys" $
            it "responds with 200 when logged in" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                getWithSession loginForm "/notification/keys" `shouldRespondWith` 200
        describe "/subscribe" $ do
            it "responds with 200 when subscribing to notifications" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/notification/subscribe" subDetails
                    `shouldRespondWithJson` (200, Response True)
            it "has no success when subscribing more than once" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/notification/subscribe" subDetails
                    `shouldRespondWithJson` (200, Response True)
                postWithSession loginForm "/notification/subscribe" subDetails
                    `shouldRespondWithJson` (200, Response False)
        describe "/clients" $ do
            it "responds with all clients" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/notification/subscribe" subDetails
                    `shouldRespondWithJson` (200, Response True)
                postWithSession loginForm "/notification/subscribe" subDetails2
                    `shouldRespondWithJson` (200, Response True)
                getWithSession loginForm "/notification/clients" 
                    `shouldRespondWithJson` (200, [subDetails, subDetails2])
            it "responds with success when deleting client" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/notification/subscribe" subDetails
                    `shouldRespondWithJson` (200, Response True)
                postWithSession loginForm "/notification/subscribe" subDetails2
                    `shouldRespondWithJson` (200, Response True)
                getWithSession loginForm "/notification/clientdelete?token=token2"
                    `shouldRespondWithJson` (200, True)
                getWithSession loginForm "/notification/clients" 
                    `shouldRespondWithJson` (200, [subDetails])
        describe "/clientdelete" $ do
            it "responds with false when deleting non existing client" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                getWithSession loginForm "/notification/clientdelete?token=not-exists"
                    `shouldRespondWithJson` (200, False)
            it "responds with 404 when no token supplied" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                getWithSession loginForm "/notification/clientdelete"
                    `shouldRespondWith` 404
        describe "/list" $ do
            it "responds with empty list when requesting notifications for 'new' user" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                getWithSession loginForm "/notification/list"
                    `shouldRespondWithJson` (200, [] :: [Notification])
        describe "/clearhistory" $ do
            it "responds with true when deleting history" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                getWithSession loginForm "/notification/clearhistory"
                    `shouldRespondWithJson` (200, True)


registerForm :: RegisterForm
registerForm = RegisterForm "test" "password" "password"

loginForm :: LoginForm
loginForm = LoginForm "test" "password"

subDetails :: SubscriptionDetails
subDetails = SubscriptionDetails "endpoint" "hash" "token" "device" "browser"

subDetails2 :: SubscriptionDetails
subDetails2 = SubscriptionDetails "endpoint2" "hash2" "token2" "device2" "browser2"