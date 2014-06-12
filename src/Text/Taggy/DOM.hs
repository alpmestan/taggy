{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.DOM where

import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Taggy.Types

import qualified Data.HashMap.Strict as HM

type AttrName = Text
type AttrValue = Text

data Element = 
  Element { eltName     :: !Text 
          , eltAttrs    :: !(HashMap AttrName AttrValue)
          , eltChildren :: [Node]
          }
  deriving (Eq, Show)

data Node =
    NodeElement Element
  | NodeContent Text
  deriving (Eq, Show)

nodeChildren :: Node -> [Node]
nodeChildren (NodeContent _) = []
nodeChildren (NodeElement e) = eltChildren e

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

