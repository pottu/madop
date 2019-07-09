module ParserSpec (spec) where

import Test.Hspec
import Text.Parsec (runParser)

import Types
import Parser


testParser :: Parser a -> String -> a 
testParser p s = 
  let parsed = runParser p InParagraph "" (s ++ "\n\n")
   in case parsed of
        Right doc -> doc
        Left e -> error ("Error when parsing \"" ++ s ++ "\"") -- Shouldn't happen.



spec :: Spec
spec = do
  describe "parseDocument" $ do
    it "handles simple document" $ do
      testParser parseDocument "# Header\n\
                               \With paragraph."
      `shouldBe`
      [Header 1 [Text "Header"], Paragraph [Text "With", Space, Text "paragraph."]]

    it "handles emphasis opened and closed in different blocks 1" $ do
      testParser parseDocument "# *Emph\n\
                               \closing*"
      `shouldBe`
      [Header 1 [Text "*", Text "Emph"], Paragraph [Text "closing", Text "*"]]

    it "handles emphasis opened and closed in different blocks 2" $ do
      testParser parseDocument "_Emph\n\n\
                               \closing_"
      `shouldBe`
      [Paragraph [Text "_", Text "Emph"], Paragraph [Text "closing", Text "_"]]

    it "handles paragraphs and code blocks" $ do
      testParser parseDocument "Some code:\n\n\
                               \    fun foo:\n\
                               \      doStuff\n\
                               \    end"
      `shouldBe`
      [Paragraph [Text "Some", Space, Text "code:"], CodeBlock ["fun foo:", "  doStuff", "end"]]

    it "handles multiple following code blocks" $ do
      testParser parseDocument "    fun foo:\n\
                               \      doStuff\n\
                               \    end\n\
                               \\n\
                               \    fun foo:\n\
                               \      doStuff\n\
                               \    end"
      `shouldBe`
      [CodeBlock ["fun foo:", "  doStuff", "end"], CodeBlock ["fun foo:", "  doStuff", "end"]]

    it "handles code-span opened and closed in different blocks 1" $ do
      testParser parseDocument "`Open\n\nClose`"
      `shouldBe`
      [Paragraph [Text "`", Text "Open"], Paragraph [Text "Close", Text "`"]]

    it "handles code-span opened and closed in different blocks 2" $ do
      testParser parseDocument "# Header `Open\nClose`"
      `shouldBe`
      [Header 1 [Text "Header", Space, Text "`", Text "Open"], 
       Paragraph [Text "Close", Text "`"]]



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

    it "handles emphasis" $ do
      testParser parseHeader "# Has *emph*"
      `shouldBe`
      Header 1 [Text "Has", Space, Emph [Text "emph"]]

    it "handles unclosed emphasis" $ do
      testParser parseHeader "# No *emph"
      `shouldBe`
      Header 1 [Text "No", Space, Text "*", Text "emph"]



  -- Used to test parsing of multiple span elements as well.
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

    it "handles emphasis" $ do
      testParser parseParagraph "Has *emphasis*"
      `shouldBe`
      Paragraph [Text "Has", Space, Emph [Text "emphasis"]]

    it "handles emphasis spanning two lines" $ do
      testParser parseParagraph "Has *emphasis\nover lines*"
      `shouldBe`
      Paragraph [Text "Has", Space, Emph [Text "emphasis", Space, Text "over", Space, Text "lines"]]

    it "handles unclosed emphasis" $ do
      testParser parseParagraph "Fake *emphasis"
      `shouldBe`
      Paragraph [Text "Fake", Space, Text "*", Text "emphasis"]

    it "handles unclosed links" $ do
      testParser parseParagraph "This [link](is fake"
      `shouldBe`
      Paragraph [Text "This", Space, Text "[", Text "link", Text "]", Text "(", Text "is", Space, Text "fake"]

    it "handles unclosed links 2" $ do
      testParser parseParagraph "This [link is fake"
      `shouldBe`
      Paragraph [Text "This", Space, Text "[", Text "link", Space, Text "is", Space, Text "fake"]

    it "handles unclosed links 2" $ do
      testParser parseParagraph "This [link\nspans\nlines](www.com)"
      `shouldBe`
      Paragraph [Text "This", Space, Link "link spans lines" "www.com" Nothing]

    it "handles nested emphasize, outer not closed" $ do
      testParser parseParagraph "*_emph_asize"
      `shouldBe`
      Paragraph [Text "*", Emph [Text "emph"], Text "asize"]

    it "handles nested emphasize, inner not closed" $ do
      testParser parseParagraph "*_emph*asize"
      `shouldBe`
      Paragraph [Emph [Text "_", Text "emph"], Text "asize"]

    it "handles code spanning two lines" $ do
      testParser parseParagraph "Has `code\nover lines`"
      `shouldBe`
      Paragraph [Text "Has", Space, Code "code over lines"]


  describe "parseCodeBlock" $ do
    it "handles space-indented code block" $ do
      testParser parseCodeBlock "    fun pseudocode:\n\
                                \      doSomething\n\
                                \    end"
      `shouldBe`
      CodeBlock [ "fun pseudocode:"
                , "  doSomething" 
                , "end"
                ]
    it "handles tab-indented code block" $ do
      testParser parseCodeBlock "\tfun pseudocode:\n\
                                \\t  doSomething\n\
                                \\tend"
      `shouldBe`
      CodeBlock [ "fun pseudocode:"
                , "  doSomething" 
                , "end"
                ]



  describe "parseLink" $ do
    it "handles simple link" $ do
      testParser parseLink "[Simple link](www.com \"Has title\")"
      `shouldBe`
      Link "Simple link" "www.com" (Just "Has title")

    it "handles a link without title" $ do
      testParser parseLink "[Untitled](www.com)"
      `shouldBe`
      Link "Untitled" "www.com" Nothing


  
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



  describe "parseStrong" $ do
    it "handles strong with *" $ do
      testParser parseStrong "**Asterisk Strong**"
      `shouldBe`
      Strong [Text "Asterisk", Space, Text "Strong"]

    it "handles strong with _" $ do
      testParser parseStrong "__Underscore Strong__"
      `shouldBe`
      Strong [Text "Underscore", Space, Text "Strong"]

    it "handles nested strong" $ do
      testParser parseStrong "**Nest __test__**"
      `shouldBe`
      Strong [Text "Nest", Space, Strong [Text "test"]]

    it "handles emphasize in strong 1" $ do
      testParser parseStrong "**_emph_**"
      `shouldBe`
      Strong [Emph [Text "emph"]]

    it "handles emphasize in strong 2" $ do
      testParser parseStrong "***emph***"
      `shouldBe`
      Strong [Emph [Text "emph"]]



  describe "parseCode" $ do
    it "handles simple code span" $ do
      testParser parseCode "`doStuff()`"
      `shouldBe`
      Code "doStuff()"

    it "handles code span started with ```" $ do
      testParser parseCode "```doStuff()```"
      `shouldBe`
      Code "doStuff()"

    it "handles code span with ` inside" $ do
      testParser parseCode "```doStuff() `and` doMore()```"
      `shouldBe`
      Code "doStuff() `and` doMore()"

    it "ignores one space after opening & before ending" $ do
      testParser parseCode "`` `foo` ``"
      `shouldBe`
      Code "`foo`"

    it "handles a single ` inside" $ do
      testParser parseCode "`` ` ``"
      `shouldBe`
      Code "`"



