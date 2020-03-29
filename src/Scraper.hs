{-|
Module      : Scraper
Description : Page scraper

Filter and hash the content based on the given element and optional attribute.
-}
module Scraper where

import Data.Char (isSpace)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Selection
import Text.HTML.TagSoup.Tree



-- | Type synonym for String
type Element = String
-- | Type synonym for String
type SiteContent = String

-- Trim the string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | Scrape the tags and convert to list strings
scrapeElementText :: Element -> SiteContent -> [String]
scrapeElementText sel site = filter ("" /=) . map (trim . fromTagText) $ scrapeElement sel site

-- | Return all text within an element and optional attribute
scrapeElement :: Element -> SiteContent -> [Tag String]
scrapeElement s site = case firstResult tagTrees selector of
    Nothing  -> []
    Just res -> filter isTagText $ flattenTree [res]
    where
        selector = parseSelector s
        tagTrees = tagTree' $ parseTags site

-- | Scrape to find the first result matching the selector
firstResult :: [TagTree String] -> Either a Selector -> Maybe (TagTree String) 
firstResult []     _         = Nothing
firstResult _      (Left  _) = Nothing
firstResult (x:xs) (Right s) = case select s x of
                                  []    -> firstResult xs (Right s)
                                  (r:_) -> Just . content $ r
