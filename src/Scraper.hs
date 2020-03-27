{-|
Module      : Scraper
Description : Page scraper

Filter and hash the content based on the given element and optional attribute.
-}
module Scraper where

import Control.Applicative
import Data.Hashable
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Selection
import Text.HTML.TagSoup.Tree

-- | Type synonym for String
type SiteContent = String
-- | Type synonym for Int
type Nesting = Int

-- | Return full site content
scrapePage :: SiteContent -> Int
scrapePage = hash

-- | Scrape to find the element hash
scrapeElementHash :: String -> SiteContent -> Int
scrapeElementHash sel site = hash $ scrapeElementText sel site

-- | Scrape element text
scrapeElementText :: String -> SiteContent -> String
scrapeElementText sel site = foldl fTags "" $ scrapeElement sel site

-- | Fold tag function
fTags :: String -> Tag String -> String
fTags c x = case renderTags [x] of
                "" -> c
                xs -> case c of
                    "" -> xs
                    _  -> c ++ "\n" ++ xs

-- | Return all text within an element and optional attribute
scrapeElement :: String -> SiteContent -> [Tag String]
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
                                  []     -> firstResult xs (Right s)
                                  (r:rs) -> Just . content $ r
