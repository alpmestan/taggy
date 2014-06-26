{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}

module Text.Taggy.Renderer (
  Renderable(..)
) where

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

class AsMarkup a where
  toMarkup :: a -> Markup

instance AsMarkup Node where
  toMarkup = \case
    NodeContent text -> Content $ Text text
    NodeElement elmt -> toMarkup elmt

instance AsMarkup Element where
  toMarkup Element{..} = eltAttrs `toAttribute` Parent tag begin end kids
    where tag   = toStatic eltName
          begin = toStatic $ "<" <> eltName
          end   = toStatic $ "</" <> eltName <> ">"
          kids  = foldMap toMarkup eltChildren

class Renderable a where
  render :: a -> Lazy.Text

instance AsMarkup a => Renderable a where
  render = renderMarkup . toMarkup

------------------------------------------------------------------------

toAttribute :: HashMap Text Text -> (Markup -> Markup)
toAttribute = flip $ foldlWithKey' toAttribute'
  where toAttribute' html attr value = AddCustomAttribute (Text attr) (Text value) html

toStatic :: Text -> StaticString
toStatic text = StaticString (unpack text ++) (encodeUtf8 text) text
