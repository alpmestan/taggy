module Text.Taggy.Combinators (hasAttr, getAttr, innerText) where

import Prelude hiding (lookup)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Text.Taggy.DOM (Element(..), Node(..), AttrName, AttrValue)
import Data.HashMap.Strict (lookup, keys)

hasAttr :: Element -> AttrName -> Bool
hasAttr = flip elem . keys . eltAttrs

getAttr :: Element -> AttrName -> Maybe AttrValue
getAttr = flip lookup . eltAttrs

innerText :: Element -> Text
innerText = mconcat . map decons . eltChildren
  where decons (NodeElement e) = innerText e
        decons (NodeContent x) = x
