module ScraperTest where

import Data.List (intercalate)
import Test.Hspec (SpecWith, describe, it)

import Scraper

testHTML :: String
testHTML = "<html><div><div><div id=\"test\"><h1>Text</h1></div></div><!-- This is a comment --><p>html</p></div>Copyright</html>"

scraperTests :: SpecWith ()
scraperTests = do
    describe "scrapeElementText" $ do
        it "html" $
            test "html" "Text\nhtml\nCopyright"
        it "div" $
            test "div" "Text\nhtml"
        it "h1" $
            test "h1" "Text"
        it "tbody" $
            test "tbody" ""
        it "div (id=test)" $
            test "div#test" "Text"
        it "div (id=ntest)" $
            test "div#ntest" ""
        it "div (nid=test)" $
            test "div[nid=test]" ""

test :: String -> String -> Bool
test s r = intercalate "n" (scrapeElementText s testHTML) == r
