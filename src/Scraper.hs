module Scraper where

import Data.Maybe (catMaybes)
import Text.HTML.Scalpel
import Network.HTTP
import Text.HTML.TagSoup

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

-- Return all tags within a designated tag from a url
getTags :: String -> URL -> IO [Tag String]
getTags tag url = do tags <- parseTags <$> openURL url
                     return $ filterTags tag 0 tags

-- Filter tags such that they match the given element, counter to find nested elements with the same tag
filterTags :: String -> Int -> [Tag String] -> [Tag String]
filterTags _ _ []     = [] -- End of tag
filterTags s n (t:ts) | isTagOpenName  s t =     filterTags s (n + 1) ts -- <tag>, increase nesting level
                      | isTagCloseName s t =     filterTags s (n - 1) ts -- </tag>, reduce nesting level
                      | isTagComment     t =     filterTags s n       ts -- ignore comment
                      | n <= 0             =     filterTags s n       ts -- not inside designated tag
                      | otherwise          = t : filterTags s n       ts -- inside designated tag, add tag

getTitlesFromAFPSite :: IO (Maybe [String])
getTitlesFromAFPSite = scrapeURL "http://www.cs.uu.nl/docs/vakken/afp/schedule.html" titles
    where titles = chroots ("tbody" // "tr") $ do
                            elems <- texts "td"
                            return $ elems !! 2

scrapeFullSite :: [URL] -> IO (Maybe [String])

scrapeSpecificSite :: URL -> IO (Maybe String)
scrapeSpecificSite url = scrapeURL url (html "html")

scrapeElement :: URL -> Selector -> IO (Maybe [String])
scrapeElement url tag = scrapeURL url (htmls tag)