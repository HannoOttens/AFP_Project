module Config where
import Data.Text
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Trans.State(StateT)
import Servant
import Servant.Auth.Server

import Models.User as UM

-- | Context with user for protected actions
type AppContext m = StateT User (AppConfig m)

-- | Application configuration
type AppConfig m = ReaderT Config m

data Config = Config { 
      dbFile         :: String, -- | Path to the sqlite database file
      initFile       :: String, -- | Path to the file containing create table statement
      pollSchedule   :: Text,   -- | CRON-schedule for polling the websites
      cookieSettings :: CookieSettings, -- | Settings for cookies
      jwtSettings    :: JWTSettings,    -- | Settings for JWT
      authConf       :: Context '[CookieSettings, JWTSettings] -- | Combined settings for JWT and cookies
}

-- | Read/construct config
config :: IO Config
config = 
      do 
      jwtKey <- generateKey
      let cookieSets = defaultCookieSettings { cookieIsSecure  = NotSecure }
          jwtSets = defaultJWTSettings jwtKey
      return $ Config {
            dbFile = "db",
            initFile = "tables.sqlite",
            pollSchedule = "0-59 * * * *",
            authConf = (cookieSets :. jwtSets :. EmptyContext),
            cookieSettings = cookieSets,
            jwtSettings = jwtSets
      }