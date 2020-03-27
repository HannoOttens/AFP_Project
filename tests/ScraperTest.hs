module ScraperTest where

import Data.Hashable
import Test.Hspec (SpecWith, describe, it)

import Scraper

testHTML :: String
testHTML = "<html><div><div><div id=\"test\"><h1>Text</h1></div></div><!-- This is a comment --><p>html</p></div>Copyright</html>"

scraperTests :: SpecWith ()
scraperTests = do
    describe "scrapePage" $
        it "test" $
            scrapePage testHTML == hash testHTML
    describe "scrapeElement" $ do
        it "html" $
            test ("html") "TexthtmlCopyright"
        it "div" $
            test ("div") "Texthtml"
        it "h1" $
            test ("h1") "Text"
        it "tbody" $
            test ("tbody") ""
        it "div (id=test)" $
            test ("div#test") "Text"
        it "div (id=ntest)" $
            test ("div#ntest") ""
        it "div (nid=test)" $
            test ("div[nid=test]") ""

test :: String -> String -> Bool
test s r = scrapeElement s testHTML == hash r
