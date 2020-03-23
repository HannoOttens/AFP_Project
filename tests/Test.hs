
import Network.Wai (Application)
import Test.Hspec (Spec, hspec)
import Test.Hspec.Wai (with)
import Database.SQLite.Simple
import Data.String (fromString)
import System.Directory (removeFile, doesFileExist)
import Control.Monad (when)

import App (app)
import Config
import qualified DBAdapter as DB

import LoginTest (loginTests)
import NotificationTest (notificationTests)
import ScraperTest (scraperTests)
import TargetTest (targetTests)

-- | specifications which use the server
spec :: IO Application -> Spec
spec app = with app $ do
    loginTests
    notificationTests
    targetTests

-- | Run tests with use of test database
main :: IO ()
main = do
    withConnection "test-db" $ \conn -> do
        conf <- config
        initDB conn
        putStrLn "\napi running on port 8080..."
        hspec $ do 
            spec $ do
                -- Before running each test, clear all tables in the database
                resetDB conn 
                return $ app $ conf {dbFile = "test-db"}
            scraperTests
    exists <- doesFileExist "test-db"
    when exists (removeFile "test-db")
    

initDB :: Connection -> IO ()
initDB conn = do 
    initQuery <- readFile "tables.sqlite"
    mapM_ (execute_ conn) $ DB.splitQuery initQuery

resetDB :: Connection -> IO ()
resetDB conn = do
    let tables = ["NotificationTokens", "Targets", "Websites", "Notifications", "Users"]
    mapM_ (execute_ conn . fromString . (++) "DELETE FROM ") tables