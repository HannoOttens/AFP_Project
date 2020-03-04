{-# LANGUAGE KindSignatures #-}
module PostRedirect where

-- Source: https://gist.github.com/alpmestan/757094ecf9401f85c5ba367ca20b8900
import Control.Monad.Trans (liftIO)
import GHC.TypeLits
import Servant
import Servant.Auth.Server

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
  :: ToHttpApiData String
  => String -- ^ what to put in the 'Location' header
  -> AppM Handler PostRedirectHandler
redirect a = return $ (addHeader a NoContent)

redirectWithCookie
  :: ToHttpApiData String
  => Config
  -> User
  -> String -- ^ what to put in the 'Location' header
  -> AppM Handler LoginHandler
redirectWithCookie conf user a = do
  mApplyCookies <- liftIO $ acceptLogin (cookieSettings conf) (jwtSettings conf) user
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> return $ (addHeader a (applyCookies NoContent))
   