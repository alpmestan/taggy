{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.DOMSpec where

import Test.Hspec
import Text.Taggy.DOM
import Text.Taggy.Types
import Data.Monoid (mempty)
import Data.Text (Text)

spec :: Spec
spec = do
  describe "domify" $ do
    it "domifies self-closing tags" $
      [ TagOpen "br" [] True
      , TagText "pizza"
      ]
      `domifiesTo`
      [ e "br" []
      , text "pizza"
      ]
    it "domifies paired tags" $
      [ TagOpen "p" [] False
      , TagText "pizza"
      , TagClose "p"
      , TagText "olives"
      ]
      `domifiesTo`
      [ e "p" [ text "pizza" ]
      , text "olives"
      ]
    it "domifies omitted closing tags at end" $
      [ TagOpen "p" [] False
      , TagText "pizza"
      ]
      `domifiesTo`
      [ e "p" [text "pizza"]
      ]
    it "closes tags when parent closes" $
      [ TagOpen "p" [] False
      , TagOpen "em" [] False
      , TagText "pizza"
      , TagClose "p"
      ]
      `domifiesTo`
      [ e "p" [ e "em" [ text "pizza" ] ]
      ]
    it "domifies implicitly closed <p>" $
      [ TagOpen "p" [] False
      , TagText "pizza"
      , TagOpen "p" [] False
      , TagText "olives"
      ]
      `domifiesTo`
      [ e "p" [text "pizza"]
      , e "p" [text "olives"]
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
      [ e "ul"
          [ e "li"
              [ text "item 1" ]
          , e "li"
              [ text "item 2" ]
          ]
      ]
    it "domifies implicitly closed <th>, <td>, <tr>" $
      [ TagOpen "table" [] False
      , TagOpen "tr" [] False
      , TagOpen "th" [] False
      , TagOpen "td" [] False
      , TagOpen "td" [] False
      , TagOpen "tr" [] False
      , TagClose "table"
      ]
      `domifiesTo`
      [ e "table"
          [ e "tr"
              [ e "th" []
              , e "td" []
              , e "td" []
              ]
          , e "tr" []
          ]
      ]

domifiesTo :: [Tag] -> [Node] -> Bool
domifiesTo tags expected =
  domify tags == expected

e :: Text -> [Node] -> Node
e tag children = NodeElement $ Element tag mempty children

text :: Text -> Node
text txt = NodeContent txt
