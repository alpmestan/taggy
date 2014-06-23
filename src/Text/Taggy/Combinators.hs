module Text.Taggy.Combinators (hasAttr, getAttr) where

import Prelude hiding (lookup)
import Text.Taggy.DOM (Element(..), AttrName, AttrValue)
import Data.HashMap.Strict (lookup, keys)

hasAttr :: Element -> AttrName -> Bool
hasAttr = flip elem . keys . eltAttrs

getAttr :: Element -> AttrName -> Maybe AttrValue
getAttr = flip lookup . eltAttrs
