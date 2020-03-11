module DBAdapter where

import Database.SQLite.Simple
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Control.Monad.Trans.Reader as TR
import Servant
import Data.String
import Data.Maybe(listToMaybe)
import Data.List.Split

import qualified Models.Website as WM
import qualified Models.User as UM
import qualified Models.Target as TM
import Config

-- | Initialize database with tables if do not already exist
initDB :: AppConfig IO ()
initDB = do
  conf <- TR.ask
  liftIO $ do 
    initQuery <- readFile (initFile conf)
    withConnection (dbFile conf) (\conn -> mapM_ (execute_ conn) $ splitQuery initQuery)

splitQuery :: String -> [Query]
splitQuery = map fromString . splitOn ";"

execDB :: (Connection -> IO a) -> IO a
execDB f = do conf <- config
              runReaderT (exec f) conf

liftDbAction :: (Connection -> IO a) -> AppConfig Handler a
liftDbAction = mapReaderT liftIO . exec

-- | Execute an action on the database
exec :: (Connection -> IO a) -> AppConfig IO a
exec f = do 
  file <- TR.asks dbFile
  liftIO $ withConnection file f

isSuccessful :: Connection -> IO Bool
isSuccessful = fmap (== 1) . changes

{- | Add a website to the database, is it not already exists and return its ID. 
     ID field is generated by database -}
addWebsite :: WM.Website -> Connection -> IO Int
addWebsite website conn = do
    maybeWebsite <- getWebsiteByURL (WM.url website) conn
    case maybeWebsite of
      Just w  -> return $ WM.idWebsite w
      Nothing -> do 
        execute conn insertWebsite (WM.url website, WM.hash website)
        fromIntegral <$> lastInsertRowId conn
  where insertWebsite = "INSERT INTO Websites (URL, LastUpdate, Hash) VALUES (?, datetime('now'), ?)"

-- | Lookup if given URL is already in the database
getWebsiteByURL :: String -> Connection -> IO (Maybe WM.Website)
getWebsiteByURL url conn = do
    result <- query conn lookupWebsite (Only url)
    return $ listToMaybe result
  where lookupWebsite = "SELECT WebsiteID, URL, LastUpdate, Hash FROM Websites WHERE URL = ?"

-- | Returns all websites that are in the database
getWebsites :: Connection -> IO [WM.Website]
getWebsites conn = query_ conn allWebsites
  where allWebsites = "SELECT WebsiteID, URL, LastUpdate, Hash FROM Websites"

-- | Check if website hash has changed, returns True if it has changed or website is not found
checkWebsiteHash :: Int -> Int -> Connection -> IO Bool
checkWebsiteHash websiteID newHash conn = do 
    result <- query conn checkHash (websiteID, newHash) :: IO [Only Int]
    let (Only count) = head result
    return $ count == 0
  where checkHash = "SELECT COUNT() FROM Websites WHERE WebsiteID = ? AND Hash = ?"

-- | Update the hash for the given websiteID
updateWebsiteHash :: Int -> Int -> Connection -> IO Bool
updateWebsiteHash websiteID newHash conn = do
    execute conn updateHash (newHash, websiteID)
    isSuccessful conn
  where updateHash = "UPDATE Websites SET Hash = ?, LastUpdate = datetime('now') WHERE WebsiteID = ?"

-- | Check if website hash has changed, returns True if it has changed or website is not found
checkTargetHash :: Int -> Int -> Connection -> IO Bool
checkTargetHash websiteID newHash conn = do 
    result <- query conn checkHash (websiteID, newHash) :: IO [Only Int]
    let (Only count) = head result
    return $ count == 0
  where checkHash = "SELECT COUNT() FROM Targets WHERE WebsiteID = ? AND Hash = ?"

  -- | Update the hash for the given websiteID
updateTargetHash :: Int -> Int -> Connection -> IO Bool
updateTargetHash websiteID newHash conn = do
    execute conn updateHash (newHash, websiteID)
    (== 1) <$> changes conn
  where updateHash = "UPDATE Targets SET Hash = ?, WHERE WebsiteID = ?"

{-| Add a new user to the database, if username is not already in use.
     ID is generated by the database. -}
addUser :: UM.User -> Connection -> IO (Maybe Int)
addUser user conn = do
    maybeUser <- getUser (UM.username user) conn 
    case maybeUser of
      Just _  -> return Nothing
      Nothing ->  do
        execute conn insertUser (UM.username user, UM.password user)
        ident <- fromIntegral <$> lastInsertRowId conn
        return $ Just ident
  where insertUser = "INSERT INTO Users (Username, Password) VALUES (?, ?)"

-- | Lookup user with given username, if it does not exist Nothing is returned
getUser :: String -> Connection -> IO (Maybe UM.User)
getUser name conn = do
    result <- query conn lookupUser (Only name)
    return $ listToMaybe result
  where lookupUser = "SELECT UserID, Username, Password FROM Users WHERE Username = ?"

-- | Add push notification details to the database, consisting of (endpoint, p256dh, auth)
addToken :: UM.User -> (String, String, String) -> Connection -> IO Bool
addToken user (endpoint, hash, auth) conn = do
    execute conn insertToken (UM.id user, endpoint, hash, auth)
    isSuccessful conn
  where insertToken = "INSERT INTO NotificationTokens (UserID, Endpoint, P256dh, Auth) VALUES (?, ?, ?, ?)"

getTokens :: Int -> Connection -> IO [(String,String,String)]
getTokens userID conn = query conn lookupTokens (Only userID)
  where lookupTokens = "SELECT (Endpoint, P256dh, Auth) FROM NotificationTokens WHERE UserID = ?"

-- | Get all users and selectors which are subscribed on given website
getTargetsOnWebsite :: Int -> Connection -> IO [TM.Target]
getTargetsOnWebsite websiteID conn = 
    query conn lookupTargets (Only websiteID)
  where lookupTargets = "SELECT TargetID, UserID, WebsiteID, Selector FROM Targets WHERE websiteID = ?"

-- | Add given target to the database and returns if the action was successful
addTarget :: TM.Target -> Connection -> IO Bool
addTarget target conn = do execute conn insertTarget target
                           isSuccessful conn
  where insertTarget = "INSERT INTO Targets (UserID, WebsiteID, Selector) VALUES (?,?,?)"

-- | Update the website and/or selector of the given target and return if the action was successful
editTarget :: TM.Target -> Connection -> IO Bool
editTarget t conn = do execute conn updateTarget (TM.websiteID t, TM.selector t, TM.id t)
                       isSuccessful conn
  where updateTarget = "UPDATE Targets SET WebsiteID = ?, Selector = ? WHERE TargetID = ?"

-- | Delete the target with the given id from the database
removeTarget :: Int -> Connection -> IO Bool
removeTarget targetID conn = do execute conn deleteTarget (Only targetID)
                                isSuccessful conn
  where deleteTarget = "DELETE FROM Targets WHERE TargetID = ?" 

-- | Get all targets of given user 
getTargetsOfUser :: Int -> Connection -> IO [TM.Target]
getTargetsOfUser userID conn = 
    query conn lookupTargets (Only userID)
  where lookupTargets = "SELECT TargetID, UserID, WebsiteID, Selector FROM Targets WHERE UserID = ?"

-- | Checks if there are websites in the database that are not targeted, so they can be removed
removeUnusedWebsites :: Connection -> IO Int
removeUnusedWebsites conn = do execute_ conn deleteWebsites
                               changes conn
  where deleteWebsites = "DELETE FROM Websites WHERE Websites.WebsiteID NOT IN (SELECT WebsiteID FROM Targets)" 

