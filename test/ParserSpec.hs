module ParserSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import Types
import Parser


testParser :: Parser a -> String -> a 
testParser p s = 
  let parsed = parse p "" (s ++ "\n\n")
   in case parsed of
        Right doc -> doc
        Left e -> error ("Error when parsing \"" ++ s ++ "\"") -- Shouldn't happen.



spec :: Spec
spec = do
  describe "parseHeader" $ do
    it "handles simple h1" $ do
      testParser parseHeader "# Simple h1"
      `shouldBe`
      (Header 1 [Text "Simple", Space, Text "h1"])

    it "handles simple h4" $ do
      testParser parseHeader "#### Simple h4"
      `shouldBe`
      (Header 4 [Text "Simple", Space, Text "h4"])

    it "handles closing hashes" $ do
      testParser parseHeader "### Closing hashes ####"
      `shouldBe`
      (Header 3 [Text "Closing", Space, Text "hashes"])



  describe "parseParagraph" $ do
    it "handles simple sentence" $ do
        testParser parseParagraph "Lorem ipsum dolor sit amet." 
        `shouldBe` 
        (Paragraph [Text "Lorem", Space, Text "ipsum", Space, 
                    Text "dolor", Space, Text "sit", Space, Text "amet."])

    it "handles soft break" $ do
        testParser parseParagraph "Lorem ipsum\ndolor sit amet." 
        `shouldBe` 
        (Paragraph [Text "Lorem", Space, Text "ipsum", Space, 
                    Text "dolor", Space, Text "sit", Space, Text "amet."])



  describe "parseLink" $ do
    it "handles simple link" $ do
      testParser parseLink "[Simple link](www.com with title)"
      `shouldBe`
      (Link "Simple link" "www.com" "with title")

    it "handles a link without title" $ do
      testParser parseLink "[Untitled](www.com)"
      `shouldBe`
      (Link "Untitled" "www.com" "")


  
  describe "parseEmph" $ do
    it "handles emphasize with *" $ do
      testParser parseEmph "*Asterisk Emphasize*"
      `shouldBe`
      Emph [Text "Asterisk", Space, Text "Emphasize"]

    it "handles emphasize with _" $ do
      testParser parseEmph "_Underscore Emphasize_"
      `shouldBe`
      Emph [Text "Underscore", Space, Text "Emphasize"]

    it "handles nested emphasize" $ do
      testParser parseEmph "*Nest _test_*"
      `shouldBe`
      Emph [Text "Nest", Space, Emph [Text "test"]]


