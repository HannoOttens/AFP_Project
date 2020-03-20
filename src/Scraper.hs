module Scraper where

import Data.Hashable
import Text.HTML.TagSoup

type SiteContent = String
type Element     = String
type Nesting     = Int
type Tags        = [Tag String]

instance {-# OVERLAPS #-} Hashable Tags where
    hashWithSalt s x = hashWithSalt s (renderTags x)

-- Return all tags from a page
scrapePage :: SiteContent -> Int
scrapePage = hash

-- Return all tags within an element from a url
scrapeElement :: Element -> SiteContent -> Int
scrapeElement = scrapeAttribute ("", "")

-- Return all tags within an element with a given attribute from a url
scrapeAttribute :: Attribute String -> Element -> SiteContent -> Int
scrapeAttribute a e site = hash $ filterTags a e 0 (parseTags site)

-- Filter tags such that they match the given element and/or attribute, counter to find nested elements with the same tag
filterTags :: Attribute String -> Element -> Nesting -> Tags -> Tags
filterTags _ _      _ []     = []
filterTags (k, v) e 0 (t:ts) = filterTags(k, v) e (fromEnum $ isTagOpenName e t && (v == fromAttrib k t || k == "")) ts -- <e k = v>, start collecting tags, otherwise continue with rest of tags
filterTags a      e n (t:ts) | isTagOpenName  e t =     filterTags a e (n + 1) ts -- <e>, increase nesting level
                             | isTagCloseName e t =     filterTags a e (n - 1) ts -- </e>, reduce nesting level, end collecting if nesting is 0
                             | otherwise          = t : filterTags a e n       ts -- inside element, add tag
