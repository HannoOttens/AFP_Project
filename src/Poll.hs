module Poll where

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Control.Monad
import Control.Monad.Reader

import qualified DBAdapter as DB
import Config
import Scraper
import Models.Target
import Models.Website

-- Poll a website, check for changes, update hash, return site content
poll :: Website -> AppConfig IO (Bool, SiteContent)
poll ws = do
      let u = url ws
      let wid = idWebsite ws
      liftIO $ putStrLn ("Polling: " ++ u)
      site <- liftIO $ getSite u
      let h = scrapePage site
      b <- DB.exec $ DB.checkWebsiteHash wid h
      when b $ do
            _ <- DB.exec $ DB.updateWebsiteHash wid h
            return ()
      return (b, site)

-- Poll all website from the database
pollWebsites :: AppConfig IO ()
pollWebsites = do 
      ws <- DB.exec DB.getWebsites
      mapM_ pollWebsite ws

-- Poll a website
pollWebsite :: Website -> AppConfig IO ()
pollWebsite ws = do
      (b, _) <- poll ws
      when b $ do
            liftIO $ putStrLn "Website changed"
            return ()

-- Poll all targets from the database
pollTargets :: AppConfig IO ()
pollTargets = do 
      ws <- DB.exec DB.getWebsites
      mapM_ (\w -> do
            (b, s) <- poll w
            when b $ do -- Website changed, continue checking targets
                  ts <- DB.exec $ DB.getTargetsOnWebsite (idWebsite w)
                  mapM_ (\t -> pollTarget t w s) ts) ws

-- Poll a target
pollTarget :: Target -> Website -> SiteContent -> AppConfig IO ()
pollTarget t w s = do
      let (Just e) = selector t
      let wid = idWebsite w
      liftIO $ putStrLn ("Polling: " ++ url w ++ " with target " ++ e) 
      let h = scrapeElement Nothing e s 
      b <- DB.exec $ DB.checkTargetHash wid h
      when b $ do
            _ <- DB.exec $ DB.updateTargetHash wid h
            liftIO $ putStrLn "Target changed"
            return ()

-- Return site as string
getSite :: URL -> IO String
getSite u = getResponseBody =<< simpleHTTP (getRequest u)