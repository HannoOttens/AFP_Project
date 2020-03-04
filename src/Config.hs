-- | Configuration 
data Config = Config
{ 
      dbFile       :: String, -- | Path to the sqlite database file
      initFile     :: String  -- | Path to the file containing create table statement
      pollSchedule :: String  -- | CRON-schedule for polling the websites
}

config :: Config
config = Config {
      dbFile = "db",
      initFile = "tables.sqlite",
      pollSchedule = "0-59 * * * *"
}