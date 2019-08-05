module ParserSpec (spec) where

import Test.Hspec
import Text.Parsec (runParser, ParseError)
import qualified Data.Map.Strict as Map

import Types
import Parser


testParser :: Parser a -> String -> a 
testParser p s = 
  let parsed = runParser p (NotInParagraph, Map.empty) "" (s ++ "\n\n")
   in case parsed of
        Right doc -> doc
        Left e -> error $ show e--error ("Error when parsing \"" ++ s ++ "\"") 

testBadInput :: Parser a -> String -> String 
testBadInput p s = 
  let parsed = runParser p (NotInParagraph, Map.empty) "" (s ++ "\n\n")
   in case parsed of
        Right _ -> error ("String \"" ++ s ++ "\" was accepted but shouldn't be.") 
        Left e -> "Not accepted"



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

    it "handles when a header ends a paragraph block" $ do
      testParser parseDocument "A paragraph.\n# Header"
      `shouldBe`
      [Paragraph [Text "A", Space, Text "paragraph."], Header 1 [Text "Header"]]

    it "handles when a hr interrupts an emphasis element" $ do
      testParser parseDocument "Some _emph\n___\nends_ here"
      `shouldBe`
      [Paragraph [Text "Some", Space, Text "_", Text "emph"], HorizontalRule,
       Paragraph [Text "ends", Text "_", Space, Text "here"]]

    it "handles html-block in between paragraphs" $ do
      testParser parseDocument "Some text.\n\n<ul><li>Item</li></ul>\n\nMore text."
      `shouldBe`
      [Paragraph [Text "Some", Space, Text "text."], HtmlBlock "<ul><li>Item</li></ul>",
       Paragraph [Text "More", Space, Text "text."]]

    it "handles when a html-block interrupts paragraph" $ do
      testParser parseDocument "Some text.\n<ul><li>Item</li></ul>\nMore text."
      `shouldBe`
      [Paragraph [Text "Some", Space, Text "text."], HtmlBlock "<ul><li>Item</li></ul>",
       Paragraph [Text "More", Space, Text "text."]]

    it "handles blanklines at start of document" $ do
      testParser parseDocument "\n\n   \n     \n \n\n \nSome text."
      `shouldBe`
      [Paragraph [Text "Some", Space, Text "text."]] 

    it "handles arbitrary blanklines" $ do
      testParser parseDocument "\n\nOne\n    \n \nTwo\n \nThree\n \n\n"
      `shouldBe`
      [Paragraph [Text "One"], Paragraph [Text "Two"], Paragraph [Text "Three"]]


  -- FIXME: Isolate testing of reference-style links from parseDocument
  -- TODO: Reference-style links need more thorough testing
  describe "parseDocument (for reference links)" $ do
    it "handles single ref-link" $ do
      testParser parseDocument "[Example]: example.com \"Title\"\n\
                               \[Link][Example]"
      `shouldBe`
      [Paragraph [Link "Link" "example.com" (Just "Title")]]

    it "handles single ref-link without title" $ do
      testParser parseDocument "[Example]: example.com\n\
                               \[Link][Example]"
      `shouldBe`
      [Paragraph [Link "Link" "example.com" Nothing]]

    it "handles title wrapped with '" $ do
      testParser parseDocument "[Example]: example.com 'Title'\n\
                               \[Link][Example]"
      `shouldBe`
      [Paragraph [Link "Link" "example.com" (Just "Title")]]

    it "handles title wrapped with parenthesis" $ do
      testParser parseDocument "[Example]: example.com (Title)\n\
                               \[Link][Example]"
      `shouldBe`
      [Paragraph [Link "Link" "example.com" (Just "Title")]]

    it "handles optional space between reference and text" $ do
      testParser parseDocument "[Example]: example.com \"Title\"\n\
                               \[Link] [Example]"
      `shouldBe`
      [Paragraph [Link "Link" "example.com" (Just "Title")]]

    it "handles non-case sensitivity" $ do
      testParser parseDocument "[Example]: example.com \"Title\"\n\
                               \[Link] [example][Link] [EXAMPLE]"
      `shouldBe`
      [Paragraph [Link "Link" "example.com" (Just "Title")
                 ,Link "Link" "example.com" (Just "Title")]
      ]

    it "handles implicit link reference" $ do
      testParser parseDocument "[Example]: example.com \"Title\"\n\
                               \[Example][]"
      `shouldBe`
      [Paragraph [Link "Example" "example.com" (Just "Title")]]

    it "handles multiple varying ref-links" $ do
      testParser parseDocument "[1]:  one.com   \"One\"\n\
                               \[2]:  two.com   'Two'\n\
                               \[3]: <three.com> (Three)\n\
                               \[One][1][Two][2][Three][3]"
      `shouldBe`
      [Paragraph [ Link "One" "one.com" (Just "One")
                 , Link "Two" "two.com" (Just "Two")
                 , Link "Three" "three.com" (Just "Three")
                 ]
      ]



  describe "parseHeader" $ do
    it "handles simple h1 (atx)" $ do
      testParser parseHeader "# Simple h1"
      `shouldBe`
      (Header 1 [Text "Simple", Space, Text "h1"])

    it "handles simple h4 (atx)" $ do
      testParser parseHeader "#### Simple h4"
      `shouldBe`
      (Header 4 [Text "Simple", Space, Text "h4"])

    it "handles closing hashes (atx)" $ do
      testParser parseHeader "### Closing hashes ####"
      `shouldBe`
      (Header 3 [Text "Closing", Space, Text "hashes"])

    it "handles emphasis (atx)" $ do
      testParser parseHeader "# Has *emph*"
      `shouldBe`
      Header 1 [Text "Has", Space, Emph [Text "emph"]]

    it "handles unclosed emphasis (atx)" $ do
      testParser parseHeader "# No *emph"
      `shouldBe`
      Header 1 [Text "No", Space, Text "*", Text "emph"]

    it "handles simple h1 (setext)" $ do
      testParser parseHeader "Simple h1\n========="
      `shouldBe`
      Header 1 [Text "Simple", Space, Text "h1"]

    it "handles simple h2 (setext)" $ do
      testParser parseHeader "Simple h2\n---------"
      `shouldBe`
      Header 2 [Text "Simple", Space, Text "h2"]

    it "handles random underline length 1 (setext)" $ do
      testParser parseHeader "Header\n="
      `shouldBe`
      Header 1 [Text "Header"]

    it "handles random underline length 2 (setext)" $ do
      testParser parseHeader "Header\n===="
      `shouldBe`
      Header 1 [Text "Header"]

    it "handles random underline length 2 (setext)" $ do
      testParser parseHeader "Header\n============================"
      `shouldBe`
      Header 1 [Text "Header"]

    it "handles header starting with # (setext)" $ do
      testParser parseHeader "#Header\n======="
      `shouldBe`
      Header 1 [Text "#", Text "Header"]

    it "ignores underline with spaces" $ do
      testBadInput parseHeader "Header\n= = ="
      `shouldBe`
      "Not accepted"



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
        (Paragraph [Text "Lorem", Space, Text "ipsum", SoftBreak, 
                    Text "dolor", Space, Text "sit", Space, Text "amet."])

    it "handles emphasis" $ do
      testParser parseParagraph "Has *emphasis*"
      `shouldBe`
      Paragraph [Text "Has", Space, Emph [Text "emphasis"]]

    it "handles emphasis spanning two lines" $ do
      testParser parseParagraph "Has *emphasis\nover lines*"
      `shouldBe`
      Paragraph [Text "Has", Space, Emph [Text "emphasis", SoftBreak, Text "over", Space, Text "lines"]]

    -- These types of tests should now be done with testBadInput instead.
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

    it "handles backslashed escaped symbol" $ do
      testParser parseParagraph "Use \\* to *emphasize*"
      `shouldBe`
      Paragraph [Text "Use", Space, Text "*", Space, Text "to", Space,
                 Emph [Text "emphasize"]]


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

    it "ignores 2-space indented block" $ do
      testBadInput parseCodeBlock "  fun pseudocode:\n\
                                  \    doSomething\n\
                                  \  end"
      `shouldBe`
      "Not accepted"

  describe "parseHtmlBlock" $ do
    it "handles simple html block" $ do
      testParser parseHtmlBlock "<ol>\n  <li>Item</li>\n</ol>"
      `shouldBe`
      HtmlBlock "<ol>\n  <li>Item</li>\n</ol>"

    it "handles simple one-liner html block" $ do
      testParser parseHtmlBlock "<div><p>Some text</p></div>"
      `shouldBe`
      HtmlBlock "<div><p>Some text</p></div>"

    it "allows blocks to have attributes" $ do
      testParser parseHtmlBlock "<div style='color:red'><p>Red text</p></div>"
      `shouldBe`
      HtmlBlock "<div style='color:red'><p>Red text</p></div>"

    it "ignores invalid tags" $ do
      testBadInput parseHtmlBlock "<notag><p>Some text</p></notag>"
      `shouldBe`
      "Not accepted"

    it "ignores mismatched tags" $ do
      testBadInput parseHtmlBlock "<div><p>Some text</p></ul>"
      `shouldBe`
      "Not accepted"

  
  describe "parseBlockQuote" $ do
    it "handles simple block quote with paragraph inside" $ do
      testParser parseBlockQuote "> One\n> Two\n"
      `shouldBe`
      BlockQuote [Paragraph [Text "One", SoftBreak, Text "Two"]]

    it "handles multiple blocks inside" $ do
      testParser parseBlockQuote "> # Header\n> Paragraph\n> \n> ***"
      `shouldBe`
      BlockQuote [Header 1 [Text "Header"], Paragraph [Text "Paragraph"], HorizontalRule]

    it "handles fake beginning at following line" $ do
      testParser parseBlockQuote "> One\n>"
      `shouldBe`
      BlockQuote [Paragraph [Text "One"]]

    it "handles nested blockquotes" $ do
      testParser parseBlockQuote "> > One\n> > Two"
      `shouldBe`
      BlockQuote [BlockQuote [Paragraph [Text "One", SoftBreak, Text "Two"]]]

    it "handles quoted code blocks" $ do
      testParser parseBlockQuote ">     fun foo()\n>       doStuff()\n>     end"
      `shouldBe`
      BlockQuote [CodeBlock ["fun foo()", "  doStuff()", "end"]]


  describe "parseHorizontalRule" $ do
    it "handles compact hr" $ do
      testParser parseHorizontalRule "***"
      `shouldBe`
      HorizontalRule

    it "handles spaces between hr" $ do
      testParser parseHorizontalRule "* * *"
      `shouldBe`
      HorizontalRule

    it "handles multiple signs hr" $ do
      testParser parseHorizontalRule "----------------------"
      `shouldBe`
      HorizontalRule

    it "handles random spacing" $ do
      testParser parseHorizontalRule "___ _ _    _     __  _   _"
      `shouldBe`
      HorizontalRule

    it "handles opening & closing spaces" $ do
      testParser parseHorizontalRule "  *  *  *  "
      `shouldBe`
      HorizontalRule


  describe "parseLink" $ do
    it "handles simple inline link" $ do
      testParser parseLink "[Simple link](www.com \"Has title\")"
      `shouldBe`
      Link "Simple link" "www.com" (Just "Has title")

    it "handles inline link without title" $ do
      testParser parseLink "[Untitled](www.com)"
      `shouldBe`
      Link "Untitled" "www.com" Nothing

    it "handles automatic link (link)" $ do
      testParser parseLink "<www.example.com>"
      `shouldBe`
      Link "www.example.com" "www.example.com" Nothing

    -- Only valid without encoding of emails.
    it "handles automatic link (email)" $ do
      testParser parseLink "<example@mail.com>"
      `shouldBe`
      Link "example@mail.com" "mailto:example@mail.com" Nothing


  
  describe "parseImage" $ do
    it "handles simple image" $ do
      testParser parseImage "![Some img](/foo/bar.jpg \"My cool image\")"
      `shouldBe`
      Image "/foo/bar.jpg" "Some img" (Just "My cool image")

    it "handles image without title" $ do
      testParser parseImage "![Some img](/foo/bar.jpg)"
      `shouldBe`
      Image "/foo/bar.jpg" "Some img" Nothing


  
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

    it "ignores unclosed emphasis" $ do
      testBadInput parseEmph "*Open but never closed"
      `shouldBe`
      "Not accepted"

    it "ignores mismatched symbols" $ do
      testBadInput parseEmph "**Mismatch__"
      `shouldBe`
      "Not accepted"



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

    it "ignores unclosed strong" $ do
      testBadInput parseStrong "**Open but never closed"
      `shouldBe`
      "Not accepted"

    it "ignores mismatched symbols" $ do
      testBadInput parseStrong "**Mismatch__"
      `shouldBe`
      "Not accepted"


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

    it "ignores unclosed code block" $ do
      testBadInput parseEmph "`fun foo()"
      `shouldBe`
      "Not accepted"

  describe "parseSymbol" $ do
    it "handles special character" $ do
      testParser parseSymbol "*"
      `shouldBe`
      Text "*"

    it "handles backslash escaped special character" $ do
      testParser parseSymbol "\\*"
      `shouldBe`
      Text "*"
