{-# LANGUAGE LambdaCase #-}
-- |
-- Module       : Text.Taggy.DOM
-- Copyright    : (c) 2014 Alp Mestanogullari, Vikram Verma
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
--
-- Many useful combinators for querying 'Element's
-- of a DOM tree.
module Text.Taggy.Combinators (hasName, hasAttr, getAttr, innerText, (//), (/&), (/*), trees, subtrees) where

import Prelude hiding (lookup)
import Data.Monoid (mconcat)
import Control.Monad (ap, (<=<))
import Data.Text (Text)
import Text.Taggy.DOM (Element(..), Node(..), AttrName, AttrValue)
import Data.HashMap.Strict (lookup, keys)

-- | Does the given 'Element' have
--   the given name?
hasName :: Element -> Text -> Bool
hasName = (==) . eltName

-- | Does the given element have
--   an attribute with the given name (or /key/)
hasAttr :: Element -> AttrName -> Bool
hasAttr = flip elem . keys . eltAttrs

-- | Get the value for the given attribute name
--   in the given 'Element'. Returns 'Nothing' if
--   the provided 'Element' doesn't have an attribute
--   with that name.
getAttr :: Element -> AttrName -> Maybe AttrValue
getAttr = flip lookup . eltAttrs

-- | Get all the bits of raw text present
--   everywhere below the given 'Element'
--   in the DOM tree.
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
