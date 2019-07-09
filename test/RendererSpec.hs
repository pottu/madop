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

    it "handles emphasized text" $ do
      renderSpan $ Emph [Text "Emphasized", Space, Text "text"]
      `shouldBe`
      "<em>Emphasized text</em>"

    it "handles strong text" $ do
      renderSpan $ Strong [Text "Strong", Space, Text "text"]
      `shouldBe`
      "<strong>Strong text</strong>"



  describe "renderBlock" $ do
    it "renders simple h1" $ do
      renderBlock $ Header 1 [Text "Simple", Space, Text "h1"]
      `shouldBe`
      "<h1>Simple h1</h1>"

    it "renders simple h4" $ do
      renderBlock $ Header 4 [Text "Simple", Space, Text "h4"]
      `shouldBe`
      "<h4>Simple h4</h4>"

    it "renders simple paragraph" $ do
      renderBlock $ Paragraph [Text "Lorem", Space, Text "ipsum", Space, Text "dolor",
                              Space, Text "sit", Space, Text "amet."]
      `shouldBe`
      "<p>Lorem ipsum dolor sit amet.</p>"

    it "renders simple pseudocode" $ do
      renderBlock $ CodeBlock ["fun foo:", "  doBar()", "end"]
      `shouldBe`
      "<pre><code>fun foo:\n  doBar()\nend\n</code></pre>"

    it "renders simple HTML (with <, >, & conversion)" $ do
      renderBlock $ CodeBlock ["<div id=\"copy\">", "  &copy; Foo Inc.", "</div>"]
      `shouldBe`
      "<pre><code>&lt;div id=\"copy\"&gt;\n  &amp;copy; Foo Inc.\n&lt;/div&gt;\n</code></pre>"
