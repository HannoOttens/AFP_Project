module Scraper (scrapeSite, scrapePage, scrapeElements, scrapeElement) where

import Network.HTTP
import Text.HTML.TagSoup

type Element = String
type URL     = String

-- Return all tags from all pages on a site
scrapeSite :: [URL] -> IO [[Tag String]]
scrapeSite = mapM scrapePage

-- Return all tags from a page
scrapePage :: URL -> IO [Tag String]
scrapePage = scrapeElement "html"

-- Return all tags within a designated elements from a url
scrapeElements :: [Element] -> URL -> IO [[Tag String]]
scrapeElements es url = mapM (`scrapeElement` url) es

-- Return all tags within a designated element from a url
scrapeElement :: Element -> URL -> IO [Tag String]
scrapeElement e url = do tags <- parseTags <$> (getResponseBody =<< simpleHTTP (getRequest url))
                         return $ filterTags e 0 tags

-- Filter tags such that they match the given element, counter to find nested elements with the same tag
filterTags :: Element -> Int -> [Tag String] -> [Tag String]
filterTags _ _ []     = [] -- End of tag
filterTags e n (t:ts) | isTagOpenName  e t =     filterTags e (n + 1) ts -- <tag>, increase nesting level
                      | isTagCloseName e t =     filterTags e (n - 1) ts -- </tag>, reduce nesting level
                      | isTagComment     t =     filterTags e n       ts -- ignore comment
                      | n <= 0             =     filterTags e n       ts -- not inside designated tag
                      | otherwise          = t : filterTags e n       ts -- inside designated tag, add tag