module RendererSpec (spec) where

import Test.Hspec

import Types
import Renderer



spec :: Spec
spec = do
  describe "renderSpan" $ do
    it "handles text" $ do
      renderSpan $ Text "Simple text"
      `shouldBe`
      "Simple text"

    it "handles space" $ do
      renderSpan Space
      `shouldBe`
      " "
    it "handles link" $ do
      renderSpan $ Link "A link" "www.com" (Just "Some title")
      `shouldBe`
      "<a href=\"www.com\" title=\"Some title\">A link</a>"

    it "handles link without title" $ do
      renderSpan $ Link "A link" "www.com" Nothing
      `shouldBe`
      "<a href=\"www.com\">A link</a>"

    it "handles image" $ do
      renderSpan $ Image "/foo/bar.jpg" "An image" (Just "Some title")
      `shouldBe`
      "<img src=\"/foo/bar.jpg\" alt=\"An image\" title=\"Some title\" />"

    it "handles image without title" $ do
      renderSpan $ Image "www.com/foo.jpg" "An image" Nothing
      `shouldBe`
      "<img src=\"www.com/foo.jpg\" alt=\"An image\" />"

    it "handles emphasized text" $ do
      renderSpan $ Emph [Text "Emphasized", Space, Text "text"]
      `shouldBe`
      "<em>Emphasized text</em>"

    it "handles strong text" $ do
      renderSpan $ Strong [Text "Strong", Space, Text "text"]
      `shouldBe`
      "<strong>Strong text</strong>"

    it "handles code span" $ do
      renderSpan $ Code "for x in y do z"
      `shouldBe`
      "<code>for x in y do z</code>"

    it "handles code span with encoded chars" $ do
      renderSpan $ Code "<div>&copy Foo Inc.</div>"
      `shouldBe`
      "<code>&lt;div&gt;&amp;copy Foo Inc.&lt;/div&gt;</code>"

    it "renders soft break" $ do
      renderSpan SoftBreak 
      `shouldBe`
      "\n"



  describe "renderBlock" $ do
    it "renders simple h1" $ do
      renderBlock $ Header 1 [Text "Simple", Space, Text "h1"]
      `shouldBe`
      "<h1>Simple h1</h1>"

    it "renders simple h4" $ do
      renderBlock $ Header 4 [Text "Simple", Space, Text "h4"]
      `shouldBe`
      "<h4>Simple h4</h4>"

    it "renders h6 if level > 6" $ do
      renderBlock $ Header 8 [Text "Render", Space, Text "h6"]
      `shouldBe`
      "<h6>Render h6</h6>"

    it "renders simple paragraph" $ do
      renderBlock $ Paragraph [Text "Lorem", Space, Text "ipsum", Space, Text "dolor",
                              Space, Text "sit", Space, Text "amet."]
      `shouldBe`
      "<p>Lorem ipsum dolor sit amet.</p>"

    it "renders simple pseudocode" $ do
      renderBlock $ CodeBlock ["fun foo:", "  doBar()", "end"]
      `shouldBe`
      "<pre><code>fun foo:\n  doBar()\nend\n</code></pre>"

    it "renders code block (with <, >, & conversion)" $ do
      renderBlock $ CodeBlock ["<div id=\"copy\">", "  &copy; Foo Inc.", "</div>"]
      `shouldBe`
      "<pre><code>&lt;div id=\"copy\"&gt;\n  &amp;copy; Foo Inc.\n&lt;/div&gt;\n</code></pre>"

    xit "renders paragraph with <, >, & converison" $ do
      renderBlock $ Paragraph [Text "AT&T", Space, Text "has", Space,
        Text "revenue", Space, Text ">", Text "$100B", Space, Text "but",
        Space, Text "<", Text "$1000B."]
      `shouldBe`
      "<p>AT&amp;T has revenue &gt;$100B but &lt;$1000B.</p>"

    it "renders horizontal rule" $ do
      renderBlock $ HorizontalRule
      `shouldBe`
      "<hr />"

    it "renders html block" $ do
      renderBlock $ HtmlBlock "<ol>\n  <li>item</li>\n</ol>"
      `shouldBe`
      "<ol>\n  <li>item</li>\n</ol>"

    it "renders block quote" $ do
      renderBlock $ BlockQuote [Header 1 [Text "Header"], Paragraph [Text "One", SoftBreak, Text "Two"]]
      `shouldBe`
      "<blockquote>\n<h1>Header</h1>\n\n<p>One\nTwo</p>\n\n</blockquote>"

