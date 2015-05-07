{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Text.Taggy.Parser
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
--
-- Parse an HTML or XML document as a list of 'Tag's
-- with 'taggyWith' or 'run'.
module Text.Taggy.Parser
  ( taggyWith
  , run
  , -- * Internal parsers
    tagopen
  , tagclose
  , tagcomment
  , tagstyle
  , tagscript
  , tagtext
  , htmlWith
  ) where

import Control.Applicative
import Data.Attoparsec.Combinator as Atto
import Data.Attoparsec.Text       as Atto
import qualified Data.Attoparsec.Text.Lazy as AttoLT
import Data.Char
import Data.Monoid
import Text.Taggy.Entities
import Text.Taggy.Types

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector    as V

scannerFor :: T.Text -> Int -> Char -> Maybe Int
scannerFor ending = go

  where metadata :: V.Vector Char
        metadata = V.fromList . T.unpack $ ending

        go i c | i == V.length metadata          = Nothing
               | metadata `V.unsafeIndex` i == c = Just (i+1)
               | otherwise                       = Just 0

matchUntil :: T.Text -> Parser T.Text
matchUntil endStr =
  T.dropEnd (T.length endStr)
    `fmap` scan 0 (scannerFor endStr)

delimitedBy :: T.Text -> T.Text -> Parser (T.Text, T.Text, T.Text)
delimitedBy begStr endStr = do
  string begStr
  mid <- matchUntil endStr
  return (begStr, mid, endStr)

delimitedByTag :: T.Text -> Bool -> Parser (Tag, T.Text, Tag)
delimitedByTag t cventities = do
  char '<'
  string t
  (as, _) <- attributes cventities
  inside <- matchUntil $ "</" <> t <> ">"
  return (TagOpen t as False, inside, TagClose t)

tagcomment :: Parser Tag
tagcomment = do
  (_, comm, _) <- delimitedBy "<!--" "-->"
  return $ TagComment comm

tagscript :: Bool -> Parser Tag
tagscript cventities = do
  (open, scr, close) <- delimitedByTag "script" cventities
  return $ TagScript open scr close

tagstyle :: Bool -> Parser Tag
tagstyle cventities = do
  (open, st, close) <- delimitedByTag "style" cventities
  return $ TagStyle open st close

possibly :: Char -> Parser ()
possibly c =  (char c *> return ())
          <|> return ()

ident :: Parser T.Text
ident =
  takeWhile1 (\c -> isAlphaNum c || c `elem` ("-_:." :: String))

attribute_ident :: Parser T.Text
attribute_ident =
  takeWhile1 (`notElem` (">=" :: String))

tagopen :: Bool -> Parser Tag
tagopen cventities = do
  char '<'
  possibly '<'
  possibly '!'
  possibly '?'
  skipSpace
  i <- ident
  (as, autoclose) <- attributes cventities
  return $ TagOpen i as autoclose

tagclose :: Parser Tag
tagclose = do
  char '<'
  char '/'
  skipSpace
  i <- ident
  skipSpace
  possibly '>'
  return $ TagClose i

tagtext :: Bool -> Parser Tag
tagtext b = (TagText . if b then convertEntities else id) `fmap` takeWhile1 (/='<')

attributes :: Bool -> Parser ([Attribute], Bool)
attributes cventities = postProcess `fmap` go emptyL
  where
    go l =  (do autoclose <- tagends
                return (l, autoclose)
            )
        <|> ( do attr <- attribute cventities
                 go (insertL attr l)
            )

    tagends = skipSpace >> parseEnd

    parseEnd = autoClosing
           <|> ("?>" *> return False)
           <|> (">" *> return False)

    autoClosing = do
      char '/'
      skipSpace
      char '>'
      return True

    postProcess (l, b) = (toListL l, b)

attribute :: Bool -> Parser Attribute
attribute cventities = do
  skipSpace
  key <- quoted <|> attribute_ident
  value <- option "" $ fmap (if cventities then convertEntities else id) $ do
    possibly ' '
    "="
    possibly ' '
    quoted <|> singlequoted <|> unquoted
  return $ Attribute key value

  where quoted = do
          "\""
          val <- Atto.takeWhile (/='"')
          "\""
          return val

        singlequoted = do
          "'"
          val <- Atto.takeWhile (/='\'')
          "'"
          return val

        unquoted = Atto.takeTill (\c -> isSpace c || c == '>')

htmlWith :: Bool -> Parser [Tag]
htmlWith cventities = go

  where go = do
          finished <- atEnd
          if finished
            then return []
            else do t <- tag cventities
                    (t:) `fmap` go

tag :: Bool -> Parser Tag
tag cventities = (skipSpace >> tagStructured cventities) <|> tagtext cventities

tagStructured :: Bool -> Parser Tag
tagStructured b =
      tagcomment
  <|> tagscript b
  <|> tagstyle b
  <|> tagopen b
  <|> tagclose

-- | Get a list of tags from an HTML document
--   represented as a 'LT.Text' value.
--
--   The 'Bool' lets you specify whether you want
--   to convert HTML entities to their corresponding
--   unicode character. ('True' means "yes convert")
taggyWith :: Bool -> LT.Text -> [Tag]
taggyWith cventities =
    either (const []) id
  . AttoLT.eitherResult
  . AttoLT.parse (htmlWith cventities)

-- | Same as 'taggyWith' but hands you back a
--   'AttoLT.Result' from @attoparsec@
run :: Bool -> LT.Text -> AttoLT.Result [Tag]
run cventities = AttoLT.parse (htmlWith cventities)
