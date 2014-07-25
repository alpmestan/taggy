{-# LANGUAGE OverloadedStrings #-}
module Text.Taggy.EntitiesSpec where

import Test.Hspec
import Text.Taggy.Entities

spec :: Spec
spec = do
  describe "convertEntities" $ do
    it "converts &quot; to \"" $
      convertEntities "&quot;" 
        `shouldBe` "\""

    it "converts &#xe5; to å" $
      convertEntities "&#xe5;"
        `shouldBe` "å"

    it "converts &#229; to å" $
      convertEntities "&#229;"
        `shouldBe` "å"

    it "leaves alone &quot" $
      convertEntities "&quot"
        `shouldBe` "&quot"

    it "leaves alone &#xe5" $
      convertEntities "&#xe5"
        `shouldBe` "&#xe5"

    it "leaves alone &#229" $
      convertEntities "&#229"
        `shouldBe` "&#229"
