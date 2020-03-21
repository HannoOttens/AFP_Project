module TestUtils where

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Network.HTTP.Types.Method (methodPost, methodPut)
import Network.Wai.Test (SResponse)
import Test.Hspec.Wai
    ( WaiExpectation, WaiSession, matchBody, request
    , shouldRespondWith)
import Test.Hspec.Wai.Matcher
import Web.FormUrlEncoded


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