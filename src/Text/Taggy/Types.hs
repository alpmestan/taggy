{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Text.Taggy.Types
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- Core types of /taggy/.
module Text.Taggy.Types
  ( -- * 'Tag' type
    Tag(..)
  , tname
  , isTagOpen
  , isTagClose
  , isTagText
  , isTagComment
  , isTagScript
  , isTagStyle
  , tagsNamed

    , -- * 'Attribute's
      Attribute(..)
  , attrs
  , attrKey
  , attrValue

  , -- * A small difference list implementation
    L
  , emptyL
  , appL
  , insertL
  , singletonL
  , toListL
  ) where

import Data.Text (Text, toCaseFold)

-- | An attribute is just an attribute name
--   and an attribute value.
data Attribute = Attribute !Text !Text
  deriving (Show, Eq)

-- | Get the attributes of a 'Tag'.
attrs :: Tag -> [Attribute]
attrs (TagOpen _ as _) = as
attrs                _ = []

-- | Get the name of an 'Attribute'.
attrKey :: Attribute -> Text
attrKey (Attribute k _) = k

-- | Get the value of an 'Attribute'.
attrValue :: Attribute -> Text
attrValue (Attribute _ v) = v

-- | A 'Tag' can be one of the following types of tags:
--
--   * an opening tag that has a name, a list of attributes, and whether
--     it is a self-closing tag or not
--   * a closing tag with the name of the tag
--   * some raw 'Text'
--   * an HTML comment tag
--   * a @<script>...</script>@ tag
--   * a @<style>...</style>@ tag
--
-- The latter two are useful to be considered
-- separately in the parser and also lets you
-- collect these bits quite easily.
data Tag = TagOpen !Text [Attribute] !Bool -- is it a self-closing tag?
         | TagClose !Text
         | TagText !Text
         | TagComment !Text
         | TagScript !Tag !Text !Tag
         | TagStyle !Tag !Text !Tag
  deriving (Show, Eq)

-- | Name of a 'Tag'.
--
-- > tname (TagClose "a") == "a"
tname :: Tag -> Text
tname (TagOpen n _ _) = n
tname (TagClose n) = n
tname (TagText _) = ""
tname (TagComment _) = "<!-- -->"
tname (TagScript _ _ _) = "script"
tname (TagStyle _ _ _) = "style"

-- | Is this 'Tag' an opening tag?
isTagOpen :: Tag -> Bool
isTagOpen (TagOpen _ _ _) = True
isTagOpen _               = False

-- | Is this 'Tag' a closing tag?
isTagClose :: Tag -> Bool
isTagClose (TagClose _) = True
isTagClose _            = False

-- | Is this 'Tag' just some flat text?
isTagText :: Tag -> Bool
isTagText (TagText _) = True
isTagText _           = False

-- | Is this 'Tag' an HTML comment tag?
isTagComment :: Tag -> Bool
isTagComment (TagComment _) = True
isTagComment _              = False

-- | Is this 'Tag' a @<script>...</script>@ tag?
isTagScript :: Tag -> Bool
isTagScript (TagScript _ _ _) = True
isTagScript _           = False

-- | Is this 'Tag' a @<style>...</style>@ tag?
isTagStyle :: Tag -> Bool
isTagStyle (TagStyle _ _ _) = True
isTagStyle _          = False

-- | Get all the (opening) tags with the given name
tagsNamed :: Text -> [Tag] -> [Tag]
tagsNamed nam = filter (named nam)

  where named n (TagOpen t _ _) = toCaseFold n == toCaseFold t
        named _ _               = False

newtype L a = L ([a] -> [a])

emptyL :: L a
emptyL = L $ const []

appL :: L a -> L a -> L a
appL (L l1) (L l2) = L $ l1 . l2

singletonL :: a -> L a
singletonL x = L (x:)

toListL :: L a -> [a]
toListL (L f) = f []

insertL :: a -> L a -> L a
insertL x (L f) = L $ (x:) . f
