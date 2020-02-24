{-# LANGUAGE OverloadedStrings #-}

module Scraper where

    import Text.HTML.Scalpel

    getTitlesFromAFPSite :: IO (Maybe [String])
    getTitlesFromAFPSite = scrapeURL "http://www.cs.uu.nl/docs/vakken/afp/schedule.html" titles
       where titles :: Scraper String [String]
             titles = chroots ("tbody" // "tr") $ do
                                elems <- texts "td"
                                return $ elems !! 2