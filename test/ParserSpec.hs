module ParserSpec (spec) where

import Test.Hspec
import Types
import Parser


spec :: Spec
spec = do
    describe "parseMd" $ do
        it "parses a simple h1" $ do
            parseMd "# Simple h1" 
            `shouldBe` 
            [(Header 1 [Text "Simple h1"])]

        it "parses a simple h4" $ do
            parseMd "#### Simple h4" 
            `shouldBe` 
            [(Header 4 [Text "Simple h4"])]

        it "parses header with closing hashes" $ do
            parseMd "## Closing #es ###" 
            `shouldBe` 
            [(Header 2 [Text "Closing #es"])]

        it "parses a simple sentence" $ do
            parseMd "Lorem ipsum dolor sit amet." 
            `shouldBe` 
            [(Paragraph [Text "Lorem ipsum dolor sit amet."])]

        it "parses a simple link" $ do
            parseMd "[Simple Link](www.com Title)" 
            `shouldBe` 
            [(Paragraph [Link "Simple Link" "www.com" "Title"])]

        it "parses text with link" $ do
            parseMd "This is [a link](www.com A title) in text." 
            `shouldBe` 
            [ Paragraph 
                [ Text "This is "
                , Link "a link" "www.com" "A title"
                , Text " in text."
                ]
            ]
