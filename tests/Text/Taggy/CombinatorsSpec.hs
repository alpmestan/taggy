{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.CombinatorsSpec where

import Text.Taggy.Combinators
import Test.Hspec
import Text.Taggy

spec :: Spec
spec = do
  let element = (\(NodeElement e) -> e) . head . domify . taggyWith False $
        "<html xmlns=\"http://www.w3.org/1999/xhtml\">foo<bar class=\"el\">baz</bar><qux class=\"el\"></qux></html>"
  describe "hasAttr" $ do
    it "Tests whether an attribute is present." $ do
      (element `hasAttr` "xmlns") `shouldBe` True
      (element `hasAttr` "href") `shouldBe` False
  describe "getAttr" $ do
    it "Retrieves present attributes." $
      (element `getAttr` "xmlns") `shouldBe` Just "http://www.w3.org/1999/xhtml"
    it "Nothing's missing attributes." $ 
      (element `getAttr` "style") `shouldBe` Nothing
  describe "innerText" $ do
    it "Should concatenate the NodeContent of the target element and all its children." $
      innerText element `shouldBe` "foobaz"
  describe "(//)" $ do
    it "Should return all children satisfying the predicate." $ do
      let predicate = (==Just "el") . flip getAttr "class"
          result    = element // predicate
      result `shouldSatisfy` not . null
      result `shouldSatisfy` all predicate

