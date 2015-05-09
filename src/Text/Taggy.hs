{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Text.Taggy
-- Copyright    : (c) 2014 Alp Mestanogullari, Vikram Verma
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- /taggy/ is a simple package for parsing HTML (and should work with XML)
-- written on top of the <http://hackage.haskell.org/package/attoparsec attoparsec>
-- library, which makes it one of the most efficient (space and time consumption wise)
-- on hackage.
--
-- This is the root module of /taggy/. It reexports everything
-- from the package. See each module's docs for details about
-- the functions and types involved in /taggy/.
--
-- While we've been testing the parser on /many/ pages, it may still
-- be a bit rough around the edges. Let us know on <http://github.com/alpmestan/taggy/issues github>
-- if you have any problem.
--
-- If you like to look at your HTML through
-- various optical instruments, feel free to take a look at
-- the companion <http://hackage.haskell.org/package/taggy-lens taggy-lens>
-- package we've put up together.
--
-- * If you want to parse a document as list of tags
--   and go through it as some kind of stream by just picking
--   what you need, head to "Text.Taggy.Parser" and take
--   a look at 'Text.Taggy.Parser.taggyWith' and
--   'Text.Taggy.Parser.run'.
-- * If you want to parse the document as a DOM tree and
--   traverse it to find the information you need,
--   use 'Text.Taggy.DOM.parseDOM'. This is especially useful
--   when used in conjunction with <http://hackage.haskell.org/package/taggy-lens taggy-lens>.
-- * If you build some HTML manually
--   or just transform some existing DOM tree
--   and want to turn it into a 'Data.Text.Lazy.Text'
--   head to "Text.Taggy.Renderer" and look at 'Text.Taggy.Renderer.render'.
module Text.Taggy 
  ( -- * Exported modules
    module Text.Taggy.Types
  , module Text.Taggy.Parser
  , module Text.Taggy.DOM
  , module Text.Taggy.Renderer
  ) where 

import Text.Taggy.Types
import Text.Taggy.Parser
import Text.Taggy.DOM
import Text.Taggy.Renderer
