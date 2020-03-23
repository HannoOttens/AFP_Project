module TestUtils where

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header(Header)
import Network.Wai
import Network.Wai.Test hiding (request)
import Test.Hspec.Wai
    ( WaiExpectation, WaiSession, matchBody, request
    , shouldRespondWith, liftIO)
import Test.Hspec.Wai.Matcher
import Test.Hspec.Wai.Internal
import Web.FormUrlEncoded
import Web.Cookie
import qualified Data.Map.Strict as Map

import Models.Login
import Models.Register
import Models.Notification
import Models.EditTarget

-- | Send a POST request with form encoded data to the server
postForm :: (ToForm a) => ByteString -> a -> WaiSession st SResponse
postForm path =
    request methodPost path [("Content-Type", "application/x-www-form-urlencoded")] . urlEncodeAsFormStable 

-- | Same a shouldRespondWith but encodes json to bytestring
shouldRespondWithJson :: (ToJSON a)
                      => WaiSession st SResponse
                      -> (Integer, a)
                      -> WaiExpectation st
shouldRespondWithJson req (expectedStatus, expectedValue) =
    let matcher = (fromInteger expectedStatus)
                    { matchBody = bodyEquals $ encode expectedValue }
    in shouldRespondWith req matcher

requestWithSession :: LoginForm -> Method -> ByteString -> [Header] -> LB.ByteString-> WaiSession st SResponse
requestWithSession login method path headers body = do 
    app <- getApp
    liftIO $ flip runSession app $ do
        srequest $ SRequest (req methodPost header "/login") (urlEncodeAsFormStable login)
        cookie <- getClientCookies
        let newHeaders = ("X-XSRF-TOKEN", setCookieValue (cookie Map.! "XSRF-TOKEN")) : headers
        srequest $ SRequest (req method newHeaders path) body
    where
        req method headers = setPath defaultRequest {requestMethod = method, requestHeaders = headers}
        header = [("Content-Type", "application/x-www-form-urlencoded")]

getWithSession :: LoginForm -> ByteString -> WaiSession st SResponse
getWithSession login path = requestWithSession login methodGet path [] ""

postWithSession :: (ToForm a) => LoginForm -> ByteString -> a -> WaiSession st SResponse
postWithSession login path =
    requestWithSession login methodPost path [("Content-Type", "application/x-www-form-urlencoded")] . urlEncodeAsFormStable 

instance ToForm RegisterForm
instance ToForm LoginForm
instance ToForm SubscriptionDetails
instance ToForm EditTarget