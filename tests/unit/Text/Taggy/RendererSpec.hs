{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.RendererSpec where

import Test.Hspec
import Text.Taggy

spec :: Spec
spec = do
  describe "render" $ do
    let doc  = "<html xmlns=\"http://www.w3.org/1999/xhtml\">foo<bar class=\"el\">baz</bar><qux class=\"el\"><quux></quux></qux></html>"
        node = head . domify $ taggyWith False doc
        elmt = (\(NodeElement e) -> e) $ node
    it "Should render a given node." $ do
      render node `shouldBe` doc
    it "Should render a given element." $ do
      render elmt `shouldBe` doc
  describe "renderWith" $ do
    let document = "<span>I &hearts; you!</span>"
        element  = (\(NodeElement e) -> e) . head . domify $ taggyWith False document
    it "Should escape HTML entities when the first argument is True." $ do
      renderWith True element `shouldBe` "<span>I &amp;hearts; you!</span>"
    it "Shouldn't escape HTML entities when the first argument is False." $ do
      renderWith False element `shouldBe` document
