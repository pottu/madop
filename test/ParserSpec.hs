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

        it "parses a random sentence" $ do
            parseMd "Lorem ipsum dolor sit amet." 
            `shouldBe` 
            [(Paragraph [Text "Lorem ipsum dolor sit amet."])]
