module Poll where

import Network.HTTP.Client
import Control.Monad
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8(unpack)

import qualified DBAdapter as DB
import Config
import Scraper
import Models.Target
import Models.Website

-- Poll a website, check for changes, update hash, return site content
pollWebsite :: Website -> AppConfig IO (Bool, SiteContent)
pollWebsite ws = do
      let u   = url ws
      let wid = idWebsite ws
      liftIO $ putStrLn ("Polling " ++ u)
      site <- getSite u
      let h = scrapePage site
      b <- DB.exec $ DB.checkWebsiteHash wid h
      when b $ do -- Website changed, update hash
            _ <- DB.exec $ DB.updateWebsiteHash wid h
            return ()
      return (b, site)

-- Poll all targets from the database
pollTargets :: AppConfig IO ()
pollTargets = do 
      ws <- DB.exec DB.getWebsites
      mapM_ (\w -> do
            (b, s) <- pollWebsite w
            when b $ do -- Website changed, continue checking targets
                  ts <- DB.exec $ DB.getTargetsOnWebsite (idWebsite w)
                  mapM_ (\t -> pollTarget (selector t) w s) ts) ws

-- Poll a target
pollTarget :: Maybe Element -> Website -> SiteContent -> AppConfig IO ()
pollTarget Nothing _ _ = do -- Full website as target, has already been checked, notify user
      liftIO $ putStrLn "Target changed"
      return ()
pollTarget (Just e) w s = do -- Specified target, check if that has changed
      liftIO $ putStrLn ("Polling " ++ url w ++ " with target " ++ e)
      let wid = idWebsite w
      let h   = scrapeElement (e, Nothing) s 
      b <- DB.exec $ DB.checkTargetHash wid h
      when b $ do -- Target changed, update hash, notify user
            _ <- DB.exec $ DB.updateTargetHash wid h
            liftIO $ putStrLn "Target changed"
            return ()

-- Return site as string
getSite :: URL -> AppConfig IO String
getSite u = do man <- asks manager
               req <- liftIO $ parseRequest u
               response <- liftIO $ httpLbs req man
               return . unpack $ responseBody response