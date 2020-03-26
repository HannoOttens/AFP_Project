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
            test ("html", Nothing) "TexthtmlCopyright"
        it "div" $
            test ("div", Nothing) "Texthtml"
        it "h1" $
            test ("h1", Nothing) "Text"
        it "tbody" $
            test ("tbody", Nothing) ""
        it "div (id=test)" $
            test ("div", Just ("id", "test")) "Text"
        it "div (id=ntest)" $
            test ("div", Just ("id", "ntest")) ""
        it "div (nid=test)" $
            test ("div", Just ("nid", "test")) ""

test :: Selector -> String -> Bool
test s r = scrapeElement s testHTML == hash r
