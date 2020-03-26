{-|
Module      : Config
Description : Configuration manager

Configuration details.
-}
module Config where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant
import Servant.Auth.Server
import Web.WebPush

import Models.User as UM

-- | Context with user for protected actions
type AppContext m = StateT User (AppConfig m)

-- | Application configuration monad
type AppConfig m = ReaderT Config m

-- | Application configuration
data Config = Config { 
      dbFile         :: String, -- ^ Path to the sqlite database file
      initFile       :: String, -- ^ Path to the file containing create table statement
      pollSchedule   :: Text,   -- ^ CRON-schedule for polling the websites
      cookieSettings :: CookieSettings, -- ^ Settings for cookies
      jwtSettings    :: JWTSettings,    -- ^ Settings for JWT
      authConf       :: Context '[CookieSettings, JWTSettings], -- ^ Combined settings for JWT and cookies
      vapidKeys      :: VAPIDKeys, -- ^ Representing a unique VAPID key pair for push notifications
      manager        :: Manager -- ^ Network connection manager, shared manager between request
}

-- | Read/construct config
config :: IO Config
config = 
      do 
      jwtKey       <- generateKey
      vapidDetails <- readVapidDetails
      connManager  <- newManager tlsManagerSettings
      let cookieSets = defaultCookieSettings { cookieIsSecure  = NotSecure
                                             , cookieXsrfSetting = Just xcrf }
          xcrf = def { xsrfExcludeGet = True }
          jwtSets = defaultJWTSettings jwtKey
          vapidKeyPair = readVAPIDKeys vapidDetails
      return $ Config {
            dbFile = "db",
            initFile = "tables.sqlite",
            pollSchedule = "0-59 * * * *",
            authConf = cookieSets :. jwtSets :. EmptyContext,
            cookieSettings = cookieSets,
            jwtSettings = jwtSets,
            vapidKeys = vapidKeyPair,
            manager = connManager
      }

readVapidDetails :: IO VAPIDKeysMinDetails
readVapidDetails = do
      file <- readFile "vapid.txt"
      let [private, x, y] = map read $ lines file
      return $ VAPIDKeysMinDetails private x y 