{-|
Module      : Poll
Description : Target polling

Poll all targets from the database, check for changes and notify the user.
-}
module Poll where

import Control.Monad (mapM_, when)
import Control.Monad.Reader (asks, liftIO)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Hashable (hash)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Network.HTTP.Client (httpLbs, parseRequest, responseBody)

import qualified DBAdapter as DB
import qualified Models.Notification as NM
import qualified Models.Target as TM
import qualified Models.Website as WM
import Config
import Notification
import Scraper

-- | Type synonym for String
type URL = String

-- | Poll all targets from the database
pollTargets :: AppConfig IO ()
pollTargets = do 
      _  <- DB.exec DB.removeUnusedWebsites
      ws <- DB.exec DB.getWebsites
      mapM_ (\w -> do
            (changed, site) <- pollWebsite w
            -- Website changed? Continue checking targets
            when changed $ do 
                  ts <- DB.exec $ DB.getTargetsOnWebsite $ WM.idWebsite w
                  mapM_ (\t -> pollTarget t w site) ts) ws

-- | Poll a website, check for changes, update hash, return site content
pollWebsite :: WM.Website -> AppConfig IO (Bool, SiteContent)
pollWebsite w = do
      liftIO $ putStrLn $ "Polling " ++ WM.url w
      site <- getSite $ WM.url w
      let hsh = hash site
      changed <- DB.exec $ DB.checkWebsiteHash (WM.idWebsite w) hsh
      -- Website changed? update hash
      when changed $ do
            _ <- DB.exec $ DB.updateWebsiteHash (WM.idWebsite w) hsh
            return ()
      return (changed, site)

-- | Poll a target
pollTarget :: TM.Target -> WM.Website -> SiteContent -> AppConfig IO ()
pollTarget t w s = case TM.selector t of
      -- Full website as target, has already been checked, notify user
      Nothing -> notify (TM.userID t) w $ "Website " ++ WM.url w ++ " has changed!"
      -- Specified target, check if that has changed
      (Just e) -> do 
            liftIO $ putStrLn $ "Polling " ++ WM.url w ++ " with target " ++ e
            let text = intercalate "\n" $ scrapeElementText e s
                hsh  = hash text
            changed <- DB.exec $ DB.checkTargetHash (TM.id t) hsh
             -- Target changed? update hash, notify user
            when changed $ do
                  _ <- DB.exec $ DB.updateTargetHash (TM.id t) hsh
                  let old  = splitOn "\n" $ TM.content t
                      new  = splitOn "\n" text
                      diff = intercalate "\n" $ diffTarget old new
                  _ <- DB.exec $ DB.updateTargetContent (TM.id t) text
                  notify (TM.userID t) w diff

-- | Get the difference as a pretty printed string
diffTarget :: [String] -> [String] -> [String]
diffTarget []     []     = []
diffTarget as     []     = map (++ " has been removed") as
diffTarget []     bs     = map (++ " has been added")   bs
diffTarget (a:as) (b:bs) | a == b || a `elem` bs && b `elem` as     = diffTarget as     bs     -- No change or lines switched
                         | a `elem` bs = (b ++ " has been added")   : diffTarget (a:as) bs     -- b added
                         | b `elem` as = (a ++ " has been removed") : diffTarget as     (b:bs) -- a removed
                         | otherwise   = (a ++ " -> " ++ b)         : diffTarget as     bs     -- a changed to b

-- | Return site as string
getSite :: URL -> AppConfig IO String
getSite u = do
      man <- asks manager
      req <- liftIO $ parseRequest u
      response <- liftIO $ httpLbs req man
      return . unpack $ responseBody response

-- | Create a notification
notify :: Int -> WM.Website -> String -> AppConfig IO ()
notify user site msg = do
      n <- createNotificationDetails user $ NM.newNotification (WM.url site) msg (WM.url site)
      DB.exec $ DB.addNotification user (WM.idWebsite site) msg
      _ <- sendNotifications n
      return ()

