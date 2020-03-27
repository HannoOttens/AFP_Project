module TargetTest where

import Data.Aeson (decode)
import Data.String (fromString)
import Network.Wai (Application, Request(..))
import Network.Wai.Test
import Test.Hspec (SpecWith, describe, it, shouldSatisfy)
import Test.Hspec.Wai

import Models.EditTarget as ETM
import Models.FullTarget as FTM
import Models.Login
import Models.Register

import TestUtils

targetTests :: SpecWith (st, Application)
targetTests = describe "target endpoint" $ do
        describe "/update" $ do
            it "add new target responds with success" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/target/update" target1
                `shouldRespondWithJson` (200, True)
            it "update target responds with success" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/target/update" target1
                postWithSession loginForm "/target/update" target2
                    `shouldRespondWithJson` (200, True)
            it "invalid update target responds with false" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/target/update" target1
                postWithSession loginForm "/target/update" target3
                    `shouldRespondWithJson` (200, False)
            it "update target of another user responds with 401" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/target/update" target1
                postForm "/register" registerForm2 `shouldRespondWith` 301
                postWithSession loginForm2 "/target/update" target2
                    `shouldRespondWith` 401
        describe "/delete" $ do
            it "delete target responds with success" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/target/update" target1
                getWithSession loginForm "/target/delete?id=1"
                    `shouldRespondWithJson` (200, True)
            it "delete target which not exists responds with 500" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                getWithSession loginForm "/target/delete?id=1"
                    `shouldRespondWith` 500
            it "delete target of another user responds with 401" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/target/update" target1
                postForm "/register" registerForm2 `shouldRespondWith` 301
                getWithSession loginForm2 "/target/delete?id=1"
                    `shouldRespondWith` 401
        describe "/list" $ do
            it "responds with all targets of user" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                postWithSession loginForm "/target/update" target1
                postWithSession loginForm "/target/update" target4
                result <- getWithSession loginForm "/target/list"
                let (Just targets) = decode $ simpleBody result :: Maybe [FullTarget]
                liftIO $ targets `shouldSatisfy` compareTargets [target1, target4]
            it "responds with no targets" $ do
                postForm "/register" registerForm `shouldRespondWith` 301
                result <- getWithSession loginForm "/target/list"
                let (Just targets) = decode $ simpleBody result :: Maybe [FullTarget]
                liftIO $ targets `shouldSatisfy` null

compareTargets :: [EditTarget] -> [FullTarget] -> Bool
compareTargets ets fts = and $ zipWith f ets fts
    where f et ft = ETM.websiteUrl et == FTM.websiteUrl ft
                 && ETM.selector et   == FTM.selector ft

registerForm :: RegisterForm
registerForm = RegisterForm "test" "password" "password"

registerForm2 :: RegisterForm
registerForm2 = RegisterForm "test2" "password2" "password2"

loginForm :: LoginForm
loginForm = LoginForm "test" "password"

loginForm2 :: LoginForm
loginForm2 = LoginForm "test2" "password2"

target1 :: EditTarget
target1 = EditTarget 0 "http://website1.com" Nothing

target2 :: EditTarget
target2 = EditTarget 1 "http://website2.com" (Just "selector")

target3 :: EditTarget
target3 = EditTarget 100 "http://website3.com" (Just "selector")

target4 :: EditTarget
target4 = EditTarget 0 "http://website3.com" (Just "selector3")