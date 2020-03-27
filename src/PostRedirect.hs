{-|
Module      : PostRedirect
Description : Redirector for Servant

Location and cookie redirector for Servant.
-}
module PostRedirect where

-- Source: https://gist.github.com/alpmestan/757094ecf9401f85c5ba367ca20b8900
import GHC.TypeLits
import Servant
import Servant.Auth.Server

import Config

-- | Return type for a reqeuest that redirects to another page
type PostRedirect (code :: Nat) loc
  = Verb 'POST code '[JSON] (Headers '[Header "Location" loc] NoContent)
-- | Return type for a reqeuest that redirects to another page with cookies
type PostCookieRedirect (code :: Nat) loc
  = Verb 'POST code '[JSON] (Headers '[Header "Location" loc
                                      , Header "Set-Cookie" SetCookie
                                      , Header "Set-Cookie" SetCookie] NoContent)

-- | Short type synonym for 'Headers '[Header "Location" String] NoContent'
type PostRedirectHandler = Headers '[Header "Location" String] NoContent
-- | Short type synonym for complex header configuration
type LoginHandler = Headers '[Header "Location" String
                            , Header "Set-Cookie" SetCookie
                            , Header "Set-Cookie" SetCookie] NoContent

-- | Redirect to URL
redirect
  :: String -- ^ The URL to redirect to
  -> AppConfig Handler PostRedirectHandler
redirect a = return $ addHeader a NoContent