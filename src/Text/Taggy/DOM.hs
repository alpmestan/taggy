{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.DOM where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Taggy.Types

data Tree = Branch !Text [Attribute] [Tree]
          | Leaf !Tag
  deriving (Eq, Show)

domify :: [Tag] -> [Tree]
domify [] = []
domify xs = go xs


  where go [] = []
        go ts = a ++ map Leaf (take 1 b) ++ go (drop 1 b)
          where (a, b) = f ts

        f (TagScript tago scr tagc : rest) = 
          f $ [tago, TagText scr, tagc] ++ rest

        f (TagStyle tago sty tagc : rest) = 
          f $ [tago, TagText sty, tagc] ++ rest


        f (TagOpen name attrs autocl : rest) =
          case f rest of 
            (inner, []) -> ( Leaf (TagOpen name attrs autocl) : inner
                           , []
                           )
            (inner, TagClose x : ts)
              | x == name -> 
                let (a, b) = f ts in 
                  (Branch name attrs inner : a, b)
              | otherwise -> 
                ( Leaf (TagOpen name attrs autocl) : inner
                , TagClose x : ts
                )
            _ -> error "Text.Taggy.DOM.domify: shouldn't happen"

        f (TagClose x:xs) = ([], TagClose x : xs)
        f (x:xs) = (Leaf x : a, b)
          where (a,b) = f xs
        f [] = ([], [])
