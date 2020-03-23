module ScraperTest where

import Test.Hspec
import Data.Hashable
import Text.HTML.TagSoup

import Scraper

testHTML :: String
testHTML = "<html><div><div><div id=\"test\"><h1>Text</h1></div></div><!-- This is a comment --><p>html</p></div>Copyright</html>"
  
scrapeTest = do
    describe "scrapePage" $
        it "test" $
            scrapePage testHTML == hash testHTML
    describe "scrapeElement" $ do
        it "html" $
            test Nothing "html" "TexthtmlCopyright"
        it "div" $
            test Nothing "div" "Texthtml"
        it "h1" $
            test Nothing "h1" "Text"
        it "tbody" $
            test Nothing "tbody" ""
        it "div (id=test)" $
            test (Just ("id", "test")) "div" "Text"
        it "div (id=ntest)" $
            test (Just ("id", "ntest")) "div" ""
        it "div (nid=test)" $
            test (Just ("nid", "test")) "div" ""

test :: Maybe (Attribute String) -> Element -> String -> Bool
test a e r = scrapeElement a e testHTML == hash r
