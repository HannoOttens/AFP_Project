module Config where
import Data.Text

data Config = Config { 
      dbFile       :: String, -- | Path to the sqlite database file
      initFile     :: String, -- | Path to the file containing create table statement
      pollSchedule :: Text    -- | CRON-schedule for polling the websites
}
