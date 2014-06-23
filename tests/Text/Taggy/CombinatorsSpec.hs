{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.CombinatorsSpec where

import Text.Taggy.Combinators
import Test.Hspec
import Text.Taggy.DOM
import Data.HashMap.Strict

spec :: Spec
spec = do
  let element = Element "html" (fromList [("xmlns", "http://www.w3.org/1999/xhtml")]) []
  describe "hasAttr" $ do
    it "Tests whether an attribute is present." $ do
      (element `hasAttr` "xmlns") `shouldBe` True
      (element `hasAttr` "href") `shouldBe` False
  describe "getAttr" $ do
    it "Retrieves present attributes." $
      (element `getAttr` "xmlns") `shouldBe` Just "http://www.w3.org/1999/xhtml"
    it "Nothing's missing attributes." $ 
      (element `getAttr` "style") `shouldBe` Nothing
