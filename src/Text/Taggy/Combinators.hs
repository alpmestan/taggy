{-# LANGUAGE LambdaCase #-}

module Text.Taggy.Combinators (hasName, hasAttr, getAttr, innerText, (//), (/&), (/*), trees, subtrees) where

import Prelude hiding (lookup)
import Data.Monoid (mconcat)
import Control.Monad (ap, (<=<))
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
innerText = mconcat . map getContent . eltChildren
  where getContent = \case { NodeElement e -> innerText e; NodeContent x -> x }

(//) :: Element -> (Element -> Bool) -> [Element]
(//) = flip filter . trees

(/&) :: Element -> [(Element -> Bool)] -> [Element]
(/&) element [] = [element]
(/&) element (x:xs) = (/& xs) <=< filter x . catElements $ eltChildren element

(/*) :: Element -> [(Element -> Bool)] -> [Element]
(/*) element selector = concat . filter (not.null) . map (/& selector) $ trees element

trees :: Element -> [Element]
trees = ap (:) subtrees

subtrees :: Element -> [Element]
subtrees = ap (:) subtrees <=< catElements . eltChildren

isElement :: Node -> Bool
isElement = \case { NodeElement _ -> True; _ -> False }

unsafeFromElement :: Node -> Element
unsafeFromElement (NodeElement e) = e
unsafeFromElement _ = error "unsafeFromElement isn't well-defined, use with caution. ;-)"

catElements :: [Node] -> [Element]
catElements = map unsafeFromElement . filter isElement
