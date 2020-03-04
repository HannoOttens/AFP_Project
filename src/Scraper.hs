module Scraper (scrapeSite, scrapePage, scrapeElements, scrapeElement, scrapeAttributes, scrapeAttribute) where

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Text.HTML.TagSoup

type URL     = String
type Element = String
type Nesting = Int

-- Return site as string
getSite :: URL -> IO String
getSite url = getResponseBody =<< simpleHTTP (getRequest url)

-- Return all tags from all pages on a site
scrapeSite :: [URL] -> IO [String]
scrapeSite = mapM scrapePage

-- Return all tags from a page
scrapePage :: URL -> IO String
scrapePage = getSite

-- Return all tags within all elements from a url
scrapeElements :: [Element] -> URL -> IO [String]
scrapeElements es url = mapM (`scrapeElement` url) es

-- Return all tags within an element from a url
scrapeElement :: Element -> URL -> IO String
scrapeElement = scrapeAttribute ("", "")

-- Return all tags within an element with given attributes from a url
scrapeAttributes :: [Attribute String] -> Element -> URL -> IO [String]
scrapeAttributes as e url = mapM (\a -> scrapeAttribute a e url) as

-- Return all tags within an element with a given attribute from a url
scrapeAttribute :: Attribute String -> Element -> URL -> IO String
scrapeAttribute a e url = do tags <- parseTags <$> getSite url
                             return $ renderTags $ filterTags a e 0 tags

-- Filter tags such that they match the given element and/or attribute, counter to find nested elements with the same tag
filterTags :: Attribute String -> Element -> Nesting -> [Tag String] -> [Tag String]
filterTags _ _      _ []     = []
filterTags (k, v) e 0 (t:ts) = filterTags(k, v) e (fromEnum $ isTagOpenName e t && (v == fromAttrib k t || k == "")) ts -- <e k = v>, start collecting tags, otherwise continue with rest of tags
filterTags a      e n (t:ts) | isTagOpenName  e t =     filterTags a e (n + 1) ts -- <e>, increase nesting level
                             | isTagCloseName e t =     filterTags a e (n - 1) ts -- </e>, reduce nesting level, end collecting if nesting is 0
                             | otherwise          = t : filterTags a e n       ts -- inside element, add tag
