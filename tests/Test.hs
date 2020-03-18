
import Network.Wai (Application)
import Test.Hspec (Spec, hspec)
import Test.Hspec.Wai(with)
import Database.SQLite.Simple
import Data.String(fromString)

import App(app)
import Config
import qualified DBAdapter as DB

import AccountTest(accountTests)

-- | specifications which use the server
spec :: IO Application -> Spec
spec app = with app $ do
    accountTests
    

-- | Run tests with use of test database
main :: IO ()
main =
    withConnection "test-db" $ \conn -> do
        conf <- config
        initDB conn
        putStrLn "\napi running on port 8080..."
        hspec $ spec $ do
            -- Before running each test, clear all tables in the database
            resetDB conn 
            return $ app $ conf {dbFile = "test-db"}

initDB :: Connection -> IO ()
initDB conn = do 
    initQuery <- readFile "tables.sqlite"
    mapM_ (execute_ conn) $ DB.splitQuery initQuery

resetDB :: Connection -> IO ()
resetDB conn = do
    let tables = ["NotificationTokens", "Targets", "Websites", "Notifications", "Users"]
    mapM_ (execute_ conn . fromString . (++) "DELETE FROM ") tables