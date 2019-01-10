{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.DOMSpec where

import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy
import Test.Hspec
import Text.Taggy.Parser
import Text.Taggy.DOM
import Text.Taggy.Types
import Data.Monoid (mempty)

spec :: Spec
spec = do
  describe "domify" $ do
    it "domifies self-closing tags" $
      [ TagOpen "br" [] True
      , TagText "pizza"
      ]
      `domifiesTo`
      [ NodeElement (Element "br" mempty [])
      , NodeContent "pizza"
      ]
    it "domifies paired tags" $
      [ TagOpen "p" [] False
      , TagText "pizza"
      , TagClose "p"
      , TagText "olives"
      ]
      `domifiesTo`
      [ NodeElement (Element "p" mempty [NodeContent "pizza"])
      , NodeContent "olives"
      ]
    it "domifies implicitly closed <p>" $
      [ TagOpen "p" [] False
      , TagText "pizza"
      , TagOpen "p" [] False
      , TagText "olives"
      ]
      `domifiesTo`
      [ NodeElement (Element "p" mempty [NodeContent "pizza"])
      , NodeElement (Element "p" mempty [NodeContent "olives"])
      ]
    it "domifies implicitly closed <li>" $
      [ TagOpen "ul" [] False
      , TagOpen "li" [] False
      , TagText "item 1"
      , TagOpen "li" [] False
      , TagText "item 2"
      , TagClose "ul"
      ]
      `domifiesTo`
      [ NodeElement (Element "ul" mempty
          [ NodeElement (Element "li" mempty
              [ NodeContent "item 1" ]
          , NodeElement (Element "li" mempty
              [ NodeContent "item 2" ]
          ]
      ]

domifiesTo :: [Tag] -> [Node] -> Bool
domifiesTo tags expected =
  domify tags == expected
