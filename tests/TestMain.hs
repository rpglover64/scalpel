{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Text.HTML.Scalpel

import Control.Applicative
import System.Exit
import Test.HUnit

import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.Regex.TDFA


main = exit . failures =<< runTestTT (TestList [
        scrapeTests
    ,   scrapeHtmlsTests
    ,   scrapeHtmlTests
    ])

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith $ ExitFailure n

re :: String -> Text.Regex.TDFA.Regex
re = Text.Regex.TDFA.makeRegex

scrapeTests = "scrapeTests" ~: TestList [
        scrapeTest
            "<a>foo</a>"
            (Just ["<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just ["<a>foo</a>", "<a>bar</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<b><a>foo</a></b>"
            (Just ["<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<a><a>foo</a></a>"
            (Just ["<a><a>foo</a></a>", "<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<a>foo</a>"
            Nothing
            (htmls ("b" @: []))

    ,   scrapeTest
            "<a>foo"
            (Just ["<a></a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<a>foo</a><a key=\"value\">bar</a>"
            (Just ["<a key=\"value\">bar</a>"])
            (htmls ("a" @: ["key" @= "value"]))

    ,   scrapeTest
            "<a><b><c>foo</c></b></a>"
            (Just ["<c>foo</c>"])
            (htmls ("a" // "b" @: [] // "c"))

    ,   scrapeTest
            "<c><a><b>foo</b></a></c><c><a><d><b>bar</b></d></a></c><b>baz</b>"
            (Just ["<b>foo</b>", "<b>bar</b>"])
            (htmls ("a" // "b"))

    ,   scrapeTest
            "<a class=\"a b\">foo</a>"
            (Just ["<a class=\"a b\">foo</a>"])
            (htmls ("a" @: [hasClass "a"]))

    ,   scrapeTest
            "<a class=\"a b\">foo</a>"
            Nothing
            (htmls ("a" @: [hasClass "c"]))

    ,   scrapeTest
            "<a key=\"value\">foo</a>"
            (Just ["<a key=\"value\">foo</a>"])
            (htmls ("a" @: ["key" @=~ re "va(foo|bar|lu)e"]))

    ,   scrapeTest
            "<a foo=\"value\">foo</a><a bar=\"value\">bar</a>"
            (Just ["<a foo=\"value\">foo</a>", "<a bar=\"value\">bar</a>"])
            (htmls ("a" @: [Any @= "value"]))

    ,   scrapeTest
            "<a foo=\"other\">foo</a><a bar=\"value\">bar</a>"
            (Just ["<a bar=\"value\">bar</a>"])
            (htmls ("a" @: [Any @= "value"]))

    ,   scrapeTest
            "<a foo=\"value\">foo</a><b bar=\"value\">bar</b>"
            (Just ["<a foo=\"value\">foo</a>", "<b bar=\"value\">bar</b>"])
            (htmls (Any @: [Any @= "value"]))

    ,   scrapeTest
            "<a foo=\"other\">foo</a><b bar=\"value\">bar</b>"
            (Just ["<b bar=\"value\">bar</b>"])
            (htmls (Any @: [Any @= "value"]))

    ,   scrapeTest
            "<a>foo</a>"
            (Just "foo")
            (text "a")

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just "foo")
            (text "a")

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just ["foo", "bar"])
            (texts "a")

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just [True, False])
            (map (== "foo") <$> texts "a")

    ,   scrapeTest
            "<a key=foo />"
            (Just "foo")
            (attr "key" "a")

    ,   scrapeTest
            "<a key1=foo/><b key1=bar key2=foo /><a key1=bar key2=baz />"
            (Just "baz")
            (attr "key2" $ "a" @: ["key1" @= "bar"])

    ,   scrapeTest
            "<a><b>foo</b></a><b>bar</b>"
            (Just ["foo"])
            (chroot "a" $ texts "b")

    ,   scrapeTest
            "<a><b>foo</b></a><a><b>bar</b></a>"
            (Just ["foo", "bar"])
            (chroots "a" $ text "b")

    ,   scrapeTest
            "<a><b>foo</b></a><a><c>bar</c></a>"
            (Just "foo")
            (text ("a" // "b") <|> text ("a" // "c"))

    ,   scrapeTest
            "<a><b>foo</b></a><a><c>bar</c></a>"
            (Just "bar")
            (text ("a" // "d") <|> text ("a" // "c"))

    ,   scrapeTest "<img src='foobar'>" (Just "foobar") (attr "src" "img")

    ,   scrapeTest "<img src='foobar' />" (Just "foobar") (attr "src" "img")

    ,   scrapeTest
            "<a>foo</a><A>bar</A>"
            (Just ["foo", "bar"])
            (texts "a")

    ,   scrapeTest
            "<a>foo</a><A>bar</A>"
            (Just ["foo", "bar"])
            (texts "A")

    ,   scrapeTest
            "<a B=C>foo</a>"
            (Just ["foo"])
            (texts $ "A" @: ["b" @= "C"])

    ,   scrapeTest
            "<a B=C>foo</a>"
            Nothing
            (texts $ "A" @: ["b" @= "c"])
    ]

scrapeTest :: (Eq a, Show a) => String -> Maybe a -> Scraper String a -> Test
scrapeTest html expected scraper = label ~: expected @=? actual
    where
        label  = "scrape (" ++ show html ++ ")"
        actual = scrape scraper (TagSoup.parseTags html)

scrapeHtmlTests = "scrapeTests" ~: TestList [
        scrapeTest
            "<a>foo</a>"
            (Just "<a>foo</a>")
            (html "a")
    ,   scrapeTest
            "<body><div><ul><li>1</li><li>2</li></ul></div></body>"
            (Just "<li>1</li>")
            (html "li")
    ,   scrapeTest
            "<body><div></div></body>"
            (Just "<div></div>")
            (html "div")
    ]

scrapeHtmlsTests = "scrapeTests" ~: TestList [
        scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just ["<a>foo</a>","<a>bar</a>"])
            (htmls "a")
    ,   scrapeTest
            "<body><div><ul><li>1</li><li>2</li></ul></div></body>"
            (Just ["<li>1</li>", "<li>2</li>"])
            (htmls "li")
    ,   scrapeTest
            "<body><div></div></body>"
            (Just ["<div></div>"])
            (htmls "div")
    ]
