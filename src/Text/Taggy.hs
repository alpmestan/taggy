{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Text.Taggy
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- ???
module Text.Taggy 
  ( linksIn
  , module Text.Taggy.Types
  , module Text.Taggy.Parser
  ) where 

import Data.Text (Text)
import Text.Taggy.Types
import Text.Taggy.Parser

linksIn :: [Tag] -> [Text]
linksIn = map attrValue
		. filter ((=="href") . attrKey)
		. concat
		. map attrs
		. tagsNamed "a"