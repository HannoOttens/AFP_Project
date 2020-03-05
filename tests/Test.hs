import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Database.SQLite.Simple
import Data.String(fromString)

import qualified DBAdapter as DB
import Models.User as UM

genUser :: PropertyM IO User
genUser = do
    username <- pick arbitrary
    password <- pick arbitrary
    return $ User 0 username password 

prop_insert_user :: Connection -> PropertyM IO ()
prop_insert_user conn = do
  user <- genUser
  (Just personId) <- run $ DB.addUser user conn
  let userWithID = user {UM.id = personId}
  result <- run $ DB.getUser (UM.username user) conn
  assert (result == Just userWithID)

main :: IO ()
main = withConnection ":memory:" $ \connection -> do
  let 
    runSqlProperty :: IO Property -> Property
    runSqlProperty action = ioProperty $ do
        prop <- action
        resetDB connection
        return prop

    quickCheckSql :: String -> (Connection -> PropertyM IO ()) -> TestTree
    quickCheckSql name prop = testProperty name $ monadic runSqlProperty (prop connection)

  -- Initial DB setup code
  initDB connection

  -- Test as many quickcheck properties as you like
  let prop = quickCheckSql "insert user" prop_insert_user
  defaultMain prop


initDB :: Connection -> IO ()
initDB conn = do 
    initQuery <- readFile "tables.sqlite"
    mapM_ (execute_ conn) $ DB.splitQuery initQuery

resetDB :: Connection -> IO ()
resetDB conn = do
    let tables = ["NotificationTokens", "Targets", "Websites", "Notifications", "Users"]
    mapM_ (execute_ conn . fromString . (++) "DELETE FROM ") tables

