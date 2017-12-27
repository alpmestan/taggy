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
  , ParseOptions(..)
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
import Data.Default
import Data.Monoid
import Text.Taggy.Entities
import Text.Taggy.Types

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector    as V

data ParseOptions =
  ParseOptions { parseOptionConvertEntities :: Bool -- ^ Convert HTML entities to unicode characters
               , parseOptionVoidTags :: [T.Text] -- ^ tags that cannot have children
               }
  deriving (Eq, Show)

instance Default ParseOptions where
  def = ParseOptions { parseOptionConvertEntities = True
                     , parseOptionVoidTags = html5VoidTags
                     }

html5VoidTags :: [T.Text]
html5VoidTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]

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

delimitedByTag :: T.Text -> ParseOptions -> Parser (Tag, T.Text, Tag)
delimitedByTag t options = do
  char '<'
  string t
  (as, _) <- attributes options
  inside <- matchUntil $ "</" <> t <> ">"
  return (TagOpen t as False, inside, TagClose t)

tagcomment :: Parser Tag
tagcomment = do
  (_, comm, _) <- delimitedBy "<!--" "-->"
  return $ TagComment comm

tagscript :: ParseOptions -> Parser Tag
tagscript options = do
  (open, scr, close) <- delimitedByTag "script" options
  return $ TagScript open scr close

tagstyle :: ParseOptions -> Parser Tag
tagstyle options = do
  (open, st, close) <- delimitedByTag "style" options
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

tagopen :: ParseOptions -> Parser Tag
tagopen options = do
  char '<'
  possibly '<'
  possibly '!'
  possibly '?'
  skipSpace
  i <- ident
  (as, autoclose) <- attributes options
  return $ TagOpen i as (if T.toLower i `elem` parseOptionVoidTags options then True else autoclose)

tagclose :: Parser Tag
tagclose = do
  char '<'
  char '/'
  skipSpace
  i <- ident
  skipSpace
  possibly '>'
  return $ TagClose i

tagtext :: ParseOptions -> Parser Tag
tagtext options = (TagText . if parseOptionConvertEntities options then convertEntities else id) `fmap` takeWhile1 (/='<')

attributes :: ParseOptions -> Parser ([Attribute], Bool)
attributes options = postProcess `fmap` go emptyL
  where
    go l =  (do autoclose <- tagends
                return (l, autoclose)
            )
        <|> ( do attr <- attribute options
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

attribute :: ParseOptions -> Parser Attribute
attribute options = do
  skipSpace
  key <- quoted <|> attribute_ident
  value <- option "" $ fmap (if parseOptionConvertEntities options then convertEntities else id) $ do
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

htmlWith :: ParseOptions -> Parser [Tag]
htmlWith options = go

  where go = do
          finished <- atEnd
          if finished
            then return []
            else do t <- tag options
                    (t:) `fmap` go

tag :: ParseOptions -> Parser Tag
tag options = (skipSpace >> tagStructured options) <|> tagtext options

tagStructured :: ParseOptions -> Parser Tag
tagStructured options =
      tagcomment
  <|> tagscript options
  <|> tagstyle options
  <|> tagopen options
  <|> tagclose

-- | Get a list of tags from an HTML document
--   represented as a 'LT.Text' value.
taggyWith :: ParseOptions -> LT.Text -> [Tag]
taggyWith options =
    either (const []) id
  . AttoLT.eitherResult
  . AttoLT.parse (htmlWith options)

-- | Same as 'taggyWith' but hands you back a
--   'AttoLT.Result' from @attoparsec@
run :: ParseOptions -> LT.Text -> AttoLT.Result [Tag]
run options = AttoLT.parse (htmlWith options)
