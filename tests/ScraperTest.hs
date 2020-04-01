module ScraperTest where

import Test.Hspec (SpecWith, describe, it)

import Scraper

testHTML :: String
testHTML = "<html><div><div><div id=\"test\"><h1>Text</h1></div></div><!-- This is a comment --><p>html</p></div>Copyright</html>"

scraperTests :: SpecWith ()
scraperTests = do
    describe "scrapeElementText" $ do
        it "html" $
            scrapeTest "html" "Text\nhtml\nCopyright"
        it "div" $
            scrapeTest "div" "Text\nhtml"
        it "h1" $
            scrapeTest "h1" "Text"
        it "tbody" $
            scrapeTest "tbody" ""
        it "div (id=test)" $
            scrapeTest "div#test" "Text"
        it "div (id=ntest)" $
            scrapeTest "div#ntest" ""
        it "div (nid=test)" $
            scrapeTest "div[nid=test]" ""
    describe "diffTest" $ do
        it "empty" $
            diffTest "" "" ""
        it "one" $
            diffTest "hello" "hello" ""
        it "two" $
            diffTest "hello\nworld" "hello\nworld" ""
        it "added" $
            diffTest "hello" "hello\nworld" "Added:\nworld"
        it "removed" $
            diffTest "hello\nworld" "hello" "Removed:\nworld"
        it "added and removed" $
            diffTest "hello\nworld" "just\nhello" "Added:\njust\nRemoved:\nworld"
        it "changed 1" $
            diffTest "hello" "world" "Changed:\nhello -> world"
        it "changed 2" $
            diffTest "hello\nworld" "hello\nmoon" "Changed:\nworld -> moon"
        it "trio" $
            diffTest "hello\nbeautiful\nearth" "just\nhello\nmoon" "Added:\njust\nRemoved:\nearth\nChanged:\nbeautiful -> moon"

scrapeTest :: String -> String -> Bool
scrapeTest s r = scrapeElementText s testHTML == r

diffTest :: String -> String -> String -> Bool
diffTest s1 s2 d = diff "\n" s1 s2 == d
