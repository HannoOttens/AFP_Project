{-|
Module      : Scraper
Description : Page scraper

Filter and hash the content based on the given element and optional attribute.
-}

module Scraper where

import Data.Hashable
import Text.HTML.TagSoup

-- | Type synonym for String
type SiteContent = String
-- | Type synonym for String
type Selector = String
-- | Type synonym for Int
type Nesting = Int
-- | Newtype for string tag lists
newtype Tags = Tags [Tag String]
instance {-# OVERLAPS #-} Hashable Tags where
    hashWithSalt s (Tags x) = hashWithSalt s (renderTags x)

-- | Return full site content
scrapePage :: SiteContent -> Int
scrapePage = hash

-- | Return all text within an element and optional attribute
scrapeElement :: Selector -> SiteContent -> Int
scrapeElement s site = hash . Tags . filterTags s $ parseTags site

-- | Introduce nesting
filterTags :: Selector -> [Tag String] -> [Tag String]
filterTags = filterTags' 0

-- | Filter tags such that they match the given element and/or attribute, counter to find nested elements with the same tag
filterTags' :: Nesting -> Selector -> [Tag String] -> [Tag String]
filterTags' _ _ []          = []
filterTags' 0 s      (t:ts) = filterTags' (enter s t) s ts                           -- Check if nesting is increased
filterTags' n s (t:ts) | isTagText        t = t : filterTags' n       s ts -- Only text tags are relevant for site content
                       | isTagOpenName  e t =     filterTags' (n + 1) s ts -- <e>, increase nesting level
                       | isTagCloseName e t =     filterTags' (n - 1) s ts -- </e>, reduce nesting level, end collecting if nesting is 0
                       | otherwise          =     filterTags' n       s ts -- Go to next tag

-- | Determine whether the given element has been entered or not
enter :: Selector -> Tag String -> Nesting
enter s t = fromEnum $ isTagOpenName s t                        -- <e>
-- enter (e, Just (k, v)) t = fromEnum $ isTagOpenName e t && v == fromAttrib k t -- <e k = v>

