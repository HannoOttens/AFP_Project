{-# LANGUAGE KindSignatures #-}
module PostRedirect where

-- Source: https://gist.github.com/alpmestan/757094ecf9401f85c5ba367ca20b8900
import GHC.TypeLits
import Servant

type PostRedirect (code :: Nat) loc
  = Verb 'POST code '[JSON] (Headers '[Header "Location" loc] NoContent)

type PostRedirectHandler = Headers '[Header "Location" String] NoContent

redirect
  :: ToHttpApiData loc
  => loc -- ^ what to put in the 'Location' header
  -> (Headers '[Header "Location" loc] NoContent)
redirect a = (addHeader a NoContent)