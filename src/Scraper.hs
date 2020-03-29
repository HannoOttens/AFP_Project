{-|
Module      : Scraper
Description : Page scraper

Filter and the content based on the given element and optional attribute, and calculate the difference
-}
module Scraper where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Selection
import Text.HTML.TagSoup.Tree

-- | Type synonym for String
type Element = String
-- | Type synonym for String
type SiteContent = String
-- | Data type for the difference in content
data Diff = Added String | Removed String | Changed String String

-- Trim the string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | Scrape the tags and convert to list strings
scrapeElementText :: Element -> SiteContent -> String
scrapeElementText sel site = intercalate "\n" $ filter ("" /=) . map (trim . fromTagText) $ scrapeElement sel site

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

-- | Get the difference as a pretty printed string
diff :: String -> String -> String -> String
diff s a b = diffTarget (splitOn s a) (splitOn s b)

-- Make a pretty printed version of the diff
diffTarget :: [String] -> [String] -> String
diffTarget as bs = intercalate "\n" $ filter (not . null) [a, r, c]
    where
        d = diffTargetGrouped as bs
        a = printDiff "Added:"   isAdded   d
        r = printDiff "Removed:" isRemoved d
        c = printDiff "Changed:" isChanged d

-- Find three types of changes: Added, removed and changed
diffTargetGrouped :: [String] -> [String] -> [Diff]
diffTargetGrouped []     []     = []
diffTargetGrouped os     []     = map Removed os
diffTargetGrouped []     ns     = map Added   ns
diffTargetGrouped (o:os) (n:ns) | o == n      =               diffTargetGrouped os     ns -- No change
                                | o `elem` ns = Added n     : diffTargetGrouped (o:os) ns -- n added
                                | n `elem` os = Removed o   : diffTargetGrouped os (n:ns) -- o removed
                                | otherwise   = Changed o n : diffTargetGrouped os     ns -- o changed to n

-- Pretty print the difference given a certain type
printDiff :: String -> (Diff -> Bool) -> [Diff] -> String
printDiff s f ds = case map fromDiff $ filter f ds of
    [] -> ""
    xs -> intercalate "\n" (s:xs)

-- Helper functions for diffs
isAdded, isRemoved, isChanged :: Diff -> Bool
isAdded (Added _) = True
isAdded _         = False

isRemoved (Removed _) = True
isRemoved _           = False

isChanged (Changed _ _) = True
isChanged _             = False

fromDiff :: Diff -> String
fromDiff (Added a)     = quoted a
fromDiff (Removed r)   = quoted r
fromDiff (Changed o n) = quoted o ++ " -> " ++ quoted n

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""
