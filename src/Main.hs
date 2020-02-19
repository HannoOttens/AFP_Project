{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE KindSignatures #-}

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.StaticFiles

type API = StaticFile

type StaticFile = Raw

staticServer :: Server StaticFile
staticServer = serveDirectoryWebApp "var/www"

api :: Proxy API
api = Proxy

main :: IO ()
main = run 8080 . serve api $ staticServer