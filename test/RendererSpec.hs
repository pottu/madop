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
      renderSpan $ Link "A link" "www.com" "Some title"
      `shouldBe`
      "<a href=\"www.com\" title=\"Some title\">A link</a>"

    it "handles emphasized text" $ do
      renderSpan $ Emph [Text "Emphasized", Space, Text "text"]
      `shouldBe`
      "<em>Emphasized text</em>"



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
