module Text.Taggy.Combinators (hasName, hasAttr, getAttr, innerText, (//), (/&)) where

import Prelude hiding (lookup)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Text.Taggy.DOM (Element(..), Node(..), AttrName, AttrValue)
import Data.HashMap.Strict (lookup, keys)

hasName :: Element -> Text -> Bool
hasName = (==) . eltName

hasAttr :: Element -> AttrName -> Bool
hasAttr = flip elem . keys . eltAttrs

getAttr :: Element -> AttrName -> Maybe AttrValue
getAttr = flip lookup . eltAttrs

innerText :: Element -> Text
innerText = mconcat . map decons . eltChildren
  where decons (NodeElement e) = innerText e
        decons (NodeContent x) = x

(//) :: Element -> (Element -> Bool) -> [Element]
(//) = flip filter . expand
  where expand = concat . map decons . eltChildren
        decons (NodeElement e) = e : expand e
        decons _ = []

(/&) :: Element -> [(Element -> Bool)] -> [Element]
(/&) element [] = [element]
(/&) element (x:xs) = concat . map (/& xs) . filter x . immediateChildren $ eltChildren element
  where immediateChildren = map (\(NodeElement e) -> e) . filter isElement
        isElement (NodeElement _) = True
        isElement _ = False
