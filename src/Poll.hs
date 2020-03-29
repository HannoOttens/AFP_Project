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
                  _ <- DB.exec $ DB.updateTargetContent (TM.id t) text
                  let d = diff "\n" (TM.content t) text
                  notify (TM.userID t) w d

-- | Get the difference as a pretty printed string
diff :: String -> String -> String -> String
diff s a b = diffTargetGrouped (splitOn s a) (splitOn s b)

-- Make a pretty printed version of the diff
diffTargetGrouped :: [String] -> [String] -> String
diffTargetGrouped as bs = let (n, r, c) = diffTargetGrouped' as bs ([],[],[])
                              nstr = if n /= [] then "Added:\n" ++ intercalate "\n" n else ""
                              rstr = if r /= [] then "Removed:\n" ++ intercalate "\n" r else ""
                              cstr = if c /= [] then "Changed:\n" ++ (intercalate "\n" $ map (\(a, b) -> a ++ " -> " ++ b ++ "\n") c) else ""
                          in nstr ++ "\n" ++ rstr ++ "\n" ++ cstr

-- Find three types of changes: Added, removed and changed
diffTargetGrouped' :: [String] -> [String] -> ([String], [String], [(String,String)]) 
                                           -> ([String], [String], [(String,String)])
diffTargetGrouped' []     []     (n,r,c) = reverseTriple (n, r, c) 
diffTargetGrouped' as     []     (n,r,c) = reverseTriple (n, r ++ as, c) 
diffTargetGrouped' []     bs     (n,r,c) = reverseTriple (n ++ bs, r, c)
diffTargetGrouped' (a:as) (b:bs) (n,r,c) | a == b       = diffTargetGrouped' as     bs     (n,r,c)     -- No change
                                         | a `elem` bs  = diffTargetGrouped' (a:as) bs     (b:n, r, c) -- b added
                                         | b `elem` as  = diffTargetGrouped' as     (b:bs) (n, a:r, c) -- a removed
                                         | otherwise    = diffTargetGrouped' as     bs     (n, r, (a,b):c) -- a changed to b

reverseTriple :: ([a], [b], [c]) -> ([a], [b], [c])
reverseTriple (a, b, c) = (reverse a, reverse b, reverse c)


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

