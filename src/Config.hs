module Config where
import Data.Text
import Control.Monad.Trans.Reader(ReaderT)
import Servant
import Servant.Auth.Server

type AppM m = ReaderT Config m

data Config = Config { 
      dbFile         :: String, -- | Path to the sqlite database file
      initFile       :: String, -- | Path to the file containing create table statement
      pollSchedule   :: Text,   -- | CRON-schedule for polling the websites
      cookieSettings :: CookieSettings,
      jwtSettings    :: JWTSettings,
      authConf       :: Context '[CookieSettings, JWTSettings]
}

config :: IO Config
config = 
      do 
      jwtKey <- generateKey
      let cookieSets = defaultCookieSettings
          jwtSets = defaultJWTSettings jwtKey
      return $ Config {
            dbFile = "db",
            initFile = "tables.sqlite",
            pollSchedule = "0-59 * * * *",
            authConf = (cookieSets :. jwtSets :. EmptyContext),
            cookieSettings = cookieSets,
            jwtSettings = jwtSets
      }