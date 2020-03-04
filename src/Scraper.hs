module Scraper (scrapeSite, scrapePage, scrapeElements, scrapeElement, scrapeAttribute) where

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Text.HTML.TagSoup
import Data.Hashable

type URL     = String
type Element = String

getSite :: URL -> IO String
getSite url = getResponseBody =<< simpleHTTP (getRequest url)

-- Return all tags from all pages on a site
scrapeSite :: [URL] -> IO [Int]
scrapeSite = mapM scrapePage

-- Return all tags from a page
scrapePage :: URL -> IO Int
scrapePage url = do site <- getSite url
                    return $ hash site

-- Return all tags within all elements from a url
scrapeElements :: [Element] -> URL -> IO [Int]
scrapeElements es url = mapM (`scrapeElement` url) es

-- Return all tags within an element from a url
scrapeElement :: Element -> URL -> IO Int
scrapeElement e url = do tags <- parseTags <$> getSite url
                         return $ hash $ renderTags $ filterTags e ("", "") False 0 tags

-- Return all tags within an element with a given attribute from a url
scrapeAttribute :: Element -> Attribute String -> URL -> IO Int
scrapeAttribute e a url = do tags <- parseTags <$> getSite url
                             return $ hash $ renderTags $ filterTags e a False 0 tags

-- Filter tags such that they match the given element and/or attribute, counter to find nested elements with the same tag
filterTags :: Element -> Attribute String -> Bool -> Int -> [Tag String] -> [Tag String]
filterTags _ _      _     _ []     = []
filterTags e (k, v) False n (t:ts) = filterTags e (k, v) (isTagOpenName e t && (v == fromAttrib k t || k == "")) n ts -- <e k = v>, start collecting tags, otherwise continue with rest of tags
filterTags e a      True  n (t:ts) | isTagOpenName  e t =     filterTags e a True     (n + 1) ts -- <e>, increase nesting level
                                   | isTagCloseName e t =     filterTags e a (n == 1) (n - 1) ts -- </e>, reduce nesting level, end collecting if nesting is 0
                                   | otherwise          = t : filterTags e a True     n       ts -- inside element, add tag

