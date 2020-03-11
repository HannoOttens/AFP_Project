{-# LANGUAGE KindSignatures #-}
module PostRedirect where

-- Source: https://gist.github.com/alpmestan/757094ecf9401f85c5ba367ca20b8900
import GHC.TypeLits
import Servant
import Servant.Auth.Server
import Control.Monad.Trans (liftIO)

import Config
import Models.User

type PostRedirect (code :: Nat) loc
  = Verb 'POST code '[JSON] (Headers '[Header "Location" loc] NoContent)
type PostCookieRedirect (code :: Nat) loc
  = Verb 'POST code '[JSON] (Headers '[Header "Location" loc
                                      , Header "Set-Cookie" SetCookie
                                      , Header "Set-Cookie" SetCookie] NoContent)

type PostRedirectHandler = Headers '[Header "Location" String] NoContent
type LoginHandler = Headers '[Header "Location" String
                            , Header "Set-Cookie" SetCookie
                            , Header "Set-Cookie" SetCookie] NoContent

redirect
  :: String -- ^ what to put in the 'Location' header
  -> AppConfig Handler PostRedirectHandler
redirect a = return $ (addHeader a NoContent)

redirectWithCookie
  :: Config
  -> User
  -> String -- ^ what to put in the 'Location' header
  -> AppConfig Handler LoginHandler
redirectWithCookie conf user a = do
  mApplyCookies <- liftIO $ acceptLogin (cookieSettings conf) (jwtSettings conf) user
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> return $ (addHeader a (applyCookies NoContent))
   