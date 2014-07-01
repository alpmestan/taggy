{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
-- |
-- Module       : Text.Taggy.Renderer
-- Copyright    : (c) 2014 Alp Mestanogullari, Vikram Verma
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- Render a DOM tree (from "Text.Taggy.DOM")
-- using the excellent blaze markup rendering library.
module Text.Taggy.Renderer where

import Data.Foldable (Foldable(foldMap))
import Data.HashMap.Strict (HashMap, foldlWithKey')
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Blaze (Markup)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Taggy.DOM (Element(..), Node(..))
import qualified Data.Text.Lazy as Lazy (Text)
import Text.Blaze.Internal (ChoiceString(..), StaticString(..), MarkupM(..))

-- renderMarkup does entity conversion implicitly, and an override at the
-- constructor level is needed to control this; `PreEscaped (Text s)` is not
-- escaped, but a naked `Text s` is. 

class AsMarkup a where
  -- | If the first parameter is true, we align the constructors for entity
  --   conversion.
  toMarkup :: Bool -> a -> Markup

-- | A 'Node' is convertible to 'Markup'
instance AsMarkup Node where
  toMarkup convertEntities = \case
    NodeContent text -> Content $ if convertEntities then Text text else PreEscaped (Text text)
    NodeElement elmt -> toMarkup convertEntities elmt

-- | An 'Element' is convertible to 'Markup'
instance AsMarkup Element where
  toMarkup convertEntities Element{..} = eltAttrs `toAttribute` Parent tag begin end kids
    where tag   = toStatic eltName
          begin = toStatic $ "<" <> eltName
          end   = toStatic $ "</" <> eltName <> ">"
          kids  = foldMap (toMarkup convertEntities) eltChildren

class Renderable a where
  render :: a -> Lazy.Text
  render = renderWith True
  renderWith :: Bool -> a -> Lazy.Text

-- | Any value convertible to 'Markup' can be rendered as HTML, by way of
-- 'render' and 'renderWith'.

instance AsMarkup a => Renderable a where
  renderWith = fmap renderMarkup . toMarkup

toAttribute :: HashMap Text Text -> (Markup -> Markup)
toAttribute = flip $ foldlWithKey' toAttribute'
  where toAttribute' html attr value = AddCustomAttribute (Text attr) (Text value) html

toStatic :: Text -> StaticString
toStatic text = StaticString (unpack text ++) (encodeUtf8 text) text
