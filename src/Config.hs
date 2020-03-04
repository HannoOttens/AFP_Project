module Config where
import Data.Text
import Control.Monad.Trans.Reader(ReaderT)


type AppM m = ReaderT Config m

data Config = Config { 
      dbFile       :: String, -- | Path to the sqlite database file
      initFile     :: String, -- | Path to the file containing create table statement
      pollSchedule :: Text    -- | CRON-schedule for polling the websites
}
