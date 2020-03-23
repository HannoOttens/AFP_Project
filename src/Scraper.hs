module Scraper where

import Data.Hashable
import Text.HTML.TagSoup

type SiteContent = String
type Element     = String
type Nesting     = Int
newtype Tags     = Tags [Tag String]

instance {-# OVERLAPS #-} Hashable Tags where
    hashWithSalt s (Tags x) = hashWithSalt s (renderTags x)

-- Return full site content
scrapePage :: SiteContent -> Int
scrapePage = hash

-- Return all text within an element and optional attribute
scrapeElement :: Maybe (Attribute String) -> Element -> SiteContent -> Int
scrapeElement a e site = hash $ Tags $ filterTags a e (parseTags site)

-- Introduce nesting
filterTags :: Maybe (Attribute String) -> Element -> [Tag String] -> [Tag String]
filterTags = filterTags' 0

-- Filter tags such that they match the given element and/or attribute, counter to find nested elements with the same tag
filterTags' :: Nesting -> Maybe (Attribute String) -> Element -> [Tag String] -> [Tag String]
filterTags' _ _ _ []     = []
filterTags' 0 a e (t:ts) = filterTags' (enter a e t) a e ts                    -- Check if nesting is increased
filterTags' n a e (t:ts) | isTagText        t = t : filterTags' n       a e ts -- Only text tags are relevant for site content
                         | isTagOpenName  e t =     filterTags' (n + 1) a e ts -- <e>, increase nesting level
                         | isTagCloseName e t =     filterTags' (n - 1) a e ts -- </e>, reduce nesting level, end collecting if nesting is 0
                         | otherwise          =     filterTags' n       a e ts -- Go to next tag

-- Determine whether the given element has been entered or not
enter :: Maybe (Attribute String) -> Element -> Tag String -> Nesting
enter Nothing       e t = fromEnum $ isTagOpenName e t                        -- <e>
enter (Just (k, v)) e t = fromEnum $ isTagOpenName e t && v == fromAttrib k t -- <e k = v>
