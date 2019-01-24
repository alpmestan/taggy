{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
-- conjunction with <http://hackage.haskell.org/package/taggy-lens taggy-lens>

module Text.Taggy.DOM
( AttrName
, AttrValue
, Element (..)
, Node (..)
, nodeChildren
, parseDOM
, domify
, elemNode
, convertText
)
where

import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Taggy.Parser (taggyWith)
import Text.Taggy.Types
import Control.Applicative

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

-- | Helper function for building a 'HashMap' from a list of 'Attribute's
attrMap :: [Attribute] -> HashMap Text Text
attrMap attribs =
  HM.fromListWith (\v1 v2 -> v1 <> " " <> v2)
    . map attrToPair
    $ attribs
  where
    attrToPair (Attribute k v) = (k, v)

-- | Helper function to conveniently construct a 'NodeElement'
elemNode :: Text -> [Attribute] -> [Node] -> Node
elemNode name attribs children
  = NodeElement (Element name (attrMap attribs) children)

-- | Helper function to conveniently construct a 'NodeContent'
convertText :: Text -> Node
convertText t = NodeContent t

-- | Ad-hoc parser monad over a tag stream.
--
-- Essentially a state monad specialized over '[Tag]'.
newtype Domify a = Domify { runDomify :: [Tag] -> ([Tag], a) }
  deriving (Functor)

instance Applicative Domify where
  pure x =
    Domify (\tags -> (tags, x))
  liftA2 f (Domify a) (Domify b) =
    Domify $ \tags ->
      let (tags', x) = a tags
          (tags'', y) = b tags'
      in (tags'', f x y)
      
  
instance Monad Domify where
  Domify a >>= f = Domify $ \tags ->
    let (tags', x) = a tags
        Domify b = f x
    in b tags'

-- | Pop the next tag from the stream (consume). Returns 'Nothing' if the
-- end of the stream has been reached.
popTag :: Domify (Maybe Tag)
popTag = Domify $ \case
  [] -> ([], Nothing)
  (t:tags) -> (tags, Just t)

-- | Look at the next tag in the stream, but do not pop it off. Returns
-- 'Nothing' if the end of the stream has been reached.
peekTag :: Domify (Maybe Tag)
peekTag = Domify $ \case
  [] -> ([], Nothing)
  tags@(t:_) -> (tags, Just t)

-- | Transform a list of tags (produced with 'taggyWith')
--   into a list of toplevel nodes. If the document you're working
--   on is valid, there should only be one toplevel node, but let's
--   not assume we're living in an ideal world.
domify :: [Tag] -> [Node]
domify tags =
  snd $ runDomify fetchToplevelNodes tags

-- | Keep fetching 'Node's from the input stream until it is exhausted.
fetchToplevelNodes :: Domify [Node]
fetchToplevelNodes =
  peekTag >>= \case
    Nothing ->
      pure []
    Just _ -> do
      mnode <- fetchNode []
      case mnode of
        Nothing ->
          fetchToplevelNodes
        Just node ->
          (node :) <$> fetchToplevelNodes

-- | Fetch one 'Node' from the input stream. Ancestory must be given in order
-- to correctly close elements whose end tag has been omitted.
fetchNode :: [Text] -- ^ Ancestors, closest first.
          -> Domify (Maybe Node)
fetchNode ancestors = do
  popTag >>= \case
    Nothing ->
      pure Nothing -- end of input
    Just tag -> case tag of
      TagClose _ ->
        pure Nothing
      TagComment _ ->
        pure Nothing
      TagText txt ->
        pure . Just $ convertText txt
      TagOpen name attribs True ->
        pure . Just $ elemNode name attribs []
      TagOpen name attribs False -> do
        children <- fetchChildren name ancestors
        pure . Just $ elemNode name attribs children
      TagScript (TagOpen name attribs _) scr _ ->
        pure . Just $ elemNode name attribs [convertText scr]
      TagScript _ scr _ ->
        pure . Just $ elemNode "script" mempty [convertText scr]
      TagStyle (TagOpen name attribs _) sty _ ->
        pure . Just $ elemNode name attribs [convertText sty]
      TagStyle _ sty _ ->
        pure . Just $ elemNode "style" mempty [convertText sty]

-- | Fetch all children of an element. The element context is specified via the
-- current element's tag name and the ancestory chain (not including the
-- current element). That is, @fetchChildren "p" ["section", "body", "html"]@
-- fetches all the children of the @<div>@ element in the following HTML:
-- @
-- <html>
--   <body>
--     <section>
--       <div>
--         <h1>Hello</h1>
--         <p>Hello, sailor! How's it going?</p>
--       </div>
--     </section>
--   </body>
-- </html>
-- @
-- Ancestory and current node must be known in order to correctly detect the
-- end of the containing node if its end tag was omitted.
fetchChildren :: Text -> [Text] -> Domify [Node]
fetchChildren cur ancestors = do
  peekTag >>= maybe (pure []) go
  where
    -- End of document, closing shop
    go (TagClose name)
      | name == cur
      = popTag >> pure []
      | name `elem` ancestors
      = pure []
      | otherwise
      = popTag >> fetchChildren cur ancestors
    go (TagOpen name _ _)
      | name `autoCloses` cur
      = pure []
    go _
      = do
          mchild <- fetchNode (cur : ancestors)
          rest <- fetchChildren cur ancestors
          pure $ maybe rest (:rest) mchild

-- | Tells us which tags auto-close which elements. Tag name @a@ auto-closes
-- an element with tag name @b@ iff @a `autoCloses` b@.
autoCloses :: Text -> Text -> Bool
autoCloses closer closee =
  closer `elem` closersFor closee

-- | Helper for implementing 'autoCloses' in a less tedious way.
-- @closersFor a@ lists all tags @b@ for which @b `autoCloses` a@.
closersFor :: Text -> [Text]

-- An li element’s end tag may be omitted if the li element is immediately
-- followed by another li element or if there is no more content in the parent
-- element.
closersFor "li" = ["li"]

-- A dt element’s end tag may be omitted if the dt element is immediately
-- followed by another dt element or a dd element.
closersFor "dt" = ["dt", "dd"]

-- A dd element’s end tag may be omitted if the dd element is immediately
-- followed by another dd element or a dt element, or if there is no more
-- content in the parent element.
closersFor "dd" = ["dt", "dd"]

-- A p element’s end tag may be omitted if the p element is immediately
-- followed by an address, article, aside, blockquote, details, div, dl,
-- fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header,
-- hr, main, nav, ol, p, pre, section, table, or ul element, or if there is no
-- more content in the parent element and the parent element is an HTML element
-- that is not an a, audio, del, ins, map, noscript, or video element, or an
-- autonomous custom element.
closersFor "p" =
  [ "address"
  , "article"
  , "aside"
  , "blockquote"
  , "details"
  , "div"
  , "dl"
  , "fieldset"
  , "figcaption", "figure"
  , "footer"
  , "form"
  , "h1", "h2", "h3", "h4", "h5", "h6"
  , "header"
  , "hr"
  , "main"
  , "nav"
  , "ol", "ul"
  , "p"
  , "pre"
  , "section"
  , "table"
  ]

-- An rt element’s end tag may be omitted if the rt element is immediately
-- followed by an rt or rp element, or if there is no more content in the
-- parent element.
closersFor "rt" = ["rt", "rp"]

-- An rp element’s end tag may be omitted if the rp element is immediately
-- followed by an rt or rp element, or if there is no more content in the
-- parent element.
closersFor "rp" = ["rt", "rp"]

-- An optgroup element’s end tag may be omitted if the optgroup element is
-- immediately followed by another optgroup element, or if there is no more
-- content in the parent element.
closersFor "optgroup" = ["optgroup"]

-- An option element’s end tag may be omitted if the option element is
-- immediately followed by another option element, or if it is immediately
-- followed by an optgroup element, or if there is no more content in the
-- parent element.
closersFor "option" = ["option", "optgroup"]

-- A colgroup element’s start tag may be omitted if the first thing inside the
-- colgroup element is a col element, and if the element is not immediately
-- preceded by another colgroup element whose end tag has been omitted. (It
-- can’t be omitted if the element is empty.)

-- A colgroup element’s end tag may be omitted if the colgroup element is not
-- immediately followed by a space character or a comment.

-- A caption element’s end tag may be omitted if the caption element is not
-- immediately followed by a space character or a comment.

-- A thead element’s end tag may be omitted if the thead element is immediately
-- followed by a tbody or tfoot element.
closersFor "thead" = ["tbody", "tfoot", "table"]

-- A tbody element’s start tag may be omitted if the first thing inside the
-- tbody element is a tr element, and if the element is not immediately
-- preceded by a tbody, thead, or tfoot element whose end tag has been omitted.
-- (It can’t be omitted if the element is empty.)

-- A tbody element’s end tag may be omitted if the tbody element is immediately
-- followed by a tbody or tfoot element, or if there is no more content in the
-- parent element.
closersFor "tbody" = ["tbody", "tfoot", "table"]

-- A tfoot element’s end tag may be omitted if there is no more content in the
-- parent element.

-- A tr element’s end tag may be omitted if the tr element is immediately
-- followed by another tr element, or if there is no more content in the parent
-- element.
closersFor "tr" = ["tr", "tbody", "table"]

-- A td element’s end tag may be omitted if the td element is immediately
-- followed by a td or th element, or if there is no more content in the parent
-- element.
closersFor "td" = ["td", "th", "tr", "tbody", "table"]

-- A th element’s end tag may be omitted if the th element is immediately
-- followed by a td or th element, or if there is no more content in the parent
-- element.
closersFor "th" = ["td", "th", "tr", "tbody", "table"]

closersFor _ = []

