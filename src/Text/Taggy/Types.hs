-- |
-- Module       : Text.Taggy.Types
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- ???
module Text.Taggy.Types where

import Data.Text (Text, toCaseFold)

data Attribute = Attribute !Text !Text
  deriving (Show, Eq)

attrs :: Tag -> [Attribute]
attrs (TagOpen _ as _) = as
attrs                _ = []

attrKey :: Attribute -> Text
attrKey (Attribute k _) = k

attrValue :: Attribute -> Text
attrValue (Attribute _ v) = v

data Tag = TagOpen !Text [Attribute] !Bool -- is it a self-closing tag?
         | TagClose !Text
         | TagText !Text
         | TagComment !Text
         | TagScript !Tag !Text !Tag
         | TagStyle !Tag !Text !Tag
  deriving (Show, Eq)

isTagOpen :: Tag -> Bool
isTagOpen (TagOpen _ _ _) = True
isTagOpen _               = False

isTagClose :: Tag -> Bool
isTagClose (TagClose _) = True
isTagClose _            = False

isTagText :: Tag -> Bool
isTagText (TagText _) = True
isTagText _           = False

isTagComment :: Tag -> Bool
isTagComment (TagComment _) = True
isTagComment _              = False

isTagScript :: Tag -> Bool
isTagScript (TagScript _ _ _) = True
isTagScript _           = False

isTagStyle :: Tag -> Bool
isTagStyle (TagStyle _ _ _) = True
isTagStyle _          = False

tagsNamed :: Text -> [Tag] -> [Tag]
tagsNamed nam = filter (named nam)
  
  where named n (TagOpen t _ _) = toCaseFold n == toCaseFold t
        named _ _               = False

newtype L a = L { list :: [a] -> [a] }

emptyL :: L a
emptyL = L $ const []

appL :: L a -> L a -> L a
appL (L l1) (L l2) = L $ l1 . l2

singletonL :: a -> L a
singletonL x = L (x:)

toList :: L a -> [a]
toList (L f) = f []

insertL :: a -> L a -> L a
insertL x (L f) = L $ (x:) . f