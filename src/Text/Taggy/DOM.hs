{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Text.Taggy.DOM
-- Copyright    : (c) 2014 Alp Mestanogullari, Vikram Verma
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- This module will help you represent
-- an HTML or XML document as a tree
-- and let you traverse it in whatever
-- way you like.
--
-- This is especially useful when used in
-- conjunction with <http://hackage.haskell.org/package/taggy-lens taggy-lens>.
module Text.Taggy.DOM where

import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Taggy.Parser (taggyWith)
import Text.Taggy.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as LT

-- | An attribute name is just a 'Text' value
type AttrName = Text
-- | An attribute value is just a 'Text' value
type AttrValue = Text

-- | An 'Element' here refers to a tag name, the attributes
--   specified withing that tag, and all the children nodes
--   of that element. An 'Element' is basically anything but
--   \"raw\" content.
data Element = 
  Element { eltName     :: !Text -- ^ name of the element. e.g "a" for <a>
          , eltAttrs    :: !(HashMap AttrName AttrValue) -- ^ a (hash)map from attribute names to attribute values
          , eltChildren :: [Node] -- ^ children 'Node's
          }
  deriving (Eq, Show)

-- | A 'Node' is either an 'Element' or some raw text.
data Node =
    NodeElement Element
  | NodeContent Text
  deriving (Eq, Show)

-- | Get the children of a node.
--
--   If called on some raw text, this function returns @[]@.
nodeChildren :: Node -> [Node]
nodeChildren (NodeContent _) = []
nodeChildren (NodeElement e) = eltChildren e

-- | Parse an HTML or XML document
--   as a DOM tree.
--
--   The 'Bool' argument lets you specify
--   whether you want to convert HTML entities
--   to their corresponding unicode characters,
--   just like in "Text.Taggy.Parser".
--
--   > parseDOM convertEntities = domify . taggyWith cventities
parseDOM :: Bool -> LT.Text -> [Node]
parseDOM cventities =
  domify . taggyWith cventities

-- | Transform a list of tags (produced with 'taggyWith')
--   into a list of toplevel nodes. If the document you're working
--   on is valid, there should only be one toplevel node, but let's
--   not assume we're living in an ideal world.
domify :: [Tag] -> [Node]
domify [] = []
domify (TagOpen name attribs True : tags)
  = NodeElement (Element name as []) : domify tags

   where as  = HM.fromListWith (\v1 v2 -> v1 <> " " <> v2)
             . map attrToPair $ attribs

         attrToPair (Attribute k v) = (k, v)

domify (TagText txt : tags)
  = NodeContent txt : domify tags

domify (TagOpen name attribs False : tags)
  = NodeElement (Element name as cs) : domify unusedTags

  where (cs, unusedTags) = untilClosed name ([], tags)
        as  = HM.fromListWith (\v1 v2 -> v1 <> " " <> v2)
            . map attrToPair $ attribs

        attrToPair (Attribute k v) = (k, v)

domify (TagClose _ : tags) = domify tags
domify (TagComment _ : tags) = domify tags

domify (TagScript tago scr tagc : tags) =
  domify $ [tago, TagText scr, tagc] ++ tags

domify (TagStyle tago sty tagc : tags) =
  domify $ [tago, TagText sty, tagc] ++ tags

untilClosed :: Text -> ([Node], [Tag]) -> ([Node], [Tag])
untilClosed name (cousins, TagClose n : ts)
  | n == name = (cousins, ts)
  | otherwise = untilClosed name ( cousins
                                 , TagOpen n [] False
                                 : TagClose n 
                                 : ts )

untilClosed name (cousins, TagText t : ts)
  = let (cousins', ts') = untilClosed name (cousins, ts)
        cousins''       = convertText t : cousins'
    in (cousins++cousins'', ts')

untilClosed name (cousins, TagComment _ : ts)
  = untilClosed name (cousins, ts)

untilClosed name (cousins, TagOpen n as True : ts)
  = let (cousins', ts') = untilClosed name (cousins, ts)
        elt             = Element n as' []
        cousins''       = NodeElement elt : cousins'
    in (cousins++cousins'', ts')

   where as' = HM.fromListWith (\v1 v2 -> v1 <> " " <> v2)
             . map attrToPair $ as

         attrToPair (Attribute k v) = (k, v)

untilClosed name (cousins, TagOpen n as False : ts)
 = let (insideNew, ts') = untilClosed n ([], ts)
       (cousins', ts'') = untilClosed name (cousins, ts')
       elt              = Element n as' insideNew
       cousins''        = NodeElement elt : cousins'
   in (cousins'', ts'')

   where as' = HM.fromListWith (\v1 v2 -> v1 <> " " <> v2)
             . map attrToPair $ as

         attrToPair (Attribute k v) = (k, v)

untilClosed name (cousins, TagScript tago scr _ : ts)
  = let (TagOpen n at _) = tago
        (cousins', ts')  = untilClosed name (cousins, ts)
        cousins''        = NodeElement (Element n (at' at) [NodeContent scr]) : cousins'
            
    in (cousins++cousins'', ts')

   where at' at = HM.fromListWith (\v1 v2 -> v1 <> " " <> v2)
                . map attrToPair $ at

         attrToPair (Attribute k v) = (k, v)

untilClosed name (cousins, TagStyle tago sty _ : ts)
  = let (TagOpen n at _) = tago
        (cousins', ts')  = untilClosed name (cousins, ts)
        cousins''        = NodeElement (Element n (at' at) [NodeContent sty]) : cousins'
            
    in (cousins++cousins'', ts')

   where at' at = HM.fromListWith (\v1 v2 -> v1 <> " " <> v2)
                . map attrToPair $ at

         attrToPair (Attribute k v) = (k, v)

untilClosed _ (cs, []) = (cs, [])

convertText :: Text -> Node
convertText t = NodeContent t

