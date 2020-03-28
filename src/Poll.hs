{-|
Module      : Poll
Description : Target polling

Poll all targets from the database, check for changes and notify the user.
-}
module Poll where

import Control.Monad (mapM_, when)
import Control.Monad.Reader (asks, liftIO)
import Data.Algorithm.Diff
import Data.ByteString.Lazy.Char8 (unpack)
import Network.HTTP.Client (httpLbs, parseRequest, responseBody)

import qualified DBAdapter as DB
import Config
import Models.Notification (newNotification)
import Models.Target
import Models.Website
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
            (b, s) <- pollWebsite w
            when b $ do -- Website changed, continue checking targets
                  ts <- DB.exec $ DB.getTargetsOnWebsite $ idWebsite w
                  mapM_ (\t -> pollTarget t w s) ts) ws

-- | Poll a website, check for changes, update hash, return site content
pollWebsite :: Website -> AppConfig IO (Bool, SiteContent)
pollWebsite w = do
      liftIO $ putStrLn $ "Polling " ++ url w
      site <- getSite $ url w
      let h = scrapePage site
      b <- DB.exec $ DB.checkWebsiteHash (idWebsite w) h
      when b $ do -- Website changed, update hash
            _ <- DB.exec $ DB.updateWebsiteHash (idWebsite w) h
            return ()
      return (b, site)

-- | Poll a target
pollTarget :: Target -> Website -> SiteContent -> AppConfig IO ()
pollTarget t w s = case selector t of
      -- Full website as target, has already been checked, notify user
      Nothing -> notify (userID t) w $ "Website " ++ url w ++ " has changed!"
      -- Specified target, check if that has changed
      (Just e) -> do 
            liftIO $ putStrLn $ "Polling " ++ url w ++ " with target " ++ e
            let h = scrapeElementHash e s
            b <- DB.exec $ DB.checkTargetHash (idWebsite w) h
            when b $ do -- Target changed, update hash, notify user
                  _ <- DB.exec $ DB.updateTargetHash (idWebsite w) h
                  notify (userID t) w $ "Target " ++ e ++ " on website " ++ url w ++ " has changed!"

-- | Return site as string
getSite :: URL -> AppConfig IO String
getSite u = do man <- asks manager
               req <- liftIO $ parseRequest u
               response <- liftIO $ httpLbs req man
               return . unpack $ responseBody response

-- | Create a notification
notify :: Int -> Website -> String -> AppConfig IO ()
notify user site msg = do
      n <- createNotificationDetails user $ newNotification (url site) msg (url site)
      DB.exec $ DB.addNotification user (idWebsite site) msg
      _ <- sendNotifications n
      return ()
