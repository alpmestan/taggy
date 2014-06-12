{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Text.Taggy.Parser
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- ???
module Text.Taggy.Parser where 

import Control.Applicative
import Data.Attoparsec.Combinator as Atto
import Data.Attoparsec.Text       as Atto
import qualified Data.Attoparsec.Text.Lazy as AttoLT
import Data.Char
import Data.Monoid
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

delimitedByTag :: T.Text -> Parser (Tag, T.Text, Tag)
delimitedByTag t = do
  char '<'
  string t
  (as, _) <- attributes
  inside <- matchUntil $ "</" <> t <> ">"
  return (TagOpen t as False, inside, TagClose t)

tagcomment :: Parser Tag
tagcomment = do
  (_, comm, _) <- delimitedBy "<!--" "-->"
  return $ TagComment comm

tagscript :: Parser Tag
tagscript = do
  (open, scr, close) <- delimitedByTag "script"
  return $ TagScript open scr close

tagstyle :: Parser Tag
tagstyle = do
  (open, st, close) <- delimitedByTag "style"
  return $ TagStyle open st close

possibly :: Char -> Parser ()
possibly c =  (char c *> return ())
          <|> return ()

ident :: Parser T.Text
ident = takeWhile1 (\c -> isAlphaNum c || c == '-' || c == '_' || c == ':')

tagopen :: Parser Tag
tagopen = do
  char '<'
  possibly '!'
  skipSpace
  i <- ident
  (as, autoclose) <- attributes
  return $ TagOpen i as autoclose

tagclose :: Parser Tag
tagclose = do
  "</"
  skipSpace
  i <- ident
  char '>'
  return $ TagClose i

tagtext :: Parser Tag
tagtext = TagText `fmap` takeTill (=='<')

attributes :: Parser ([Attribute], Bool)
attributes = postProcess `fmap` go emptyL
  where 
    go l =  (do autoclose <- tagends 
                return (l, autoclose)
            )
        <|> ( do attr <- attribute 
                 go (insertL attr l)
            )

    tagends = skipSpace >> parseEnd

    parseEnd = ("/>" *> return True)
           <|> (">" *> return False)

    postProcess (l, b) = (toList l, b)

attribute :: Parser Attribute
attribute = do
  skipSpace
  key <- quoted <|> ident
  value <- option "" $ do 
    "="
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


html :: Parser [Tag]
html = go

  where go = do
          finished <- atEnd
          if finished
            then return []
            else do t <- tag
                    (t:) `fmap` go

tag :: Parser Tag
tag = skipSpace >> tag'

tag' :: Parser Tag
tag' = 
      tagcomment
  <|> tagscript
  <|> tagstyle
  <|> tagopen
  <|> tagclose
  <|> tagtext

tagsIn :: LT.Text -> [Tag]
tagsIn = either (const []) id
       . AttoLT.eitherResult 
       . AttoLT.parse html

run :: LT.Text -> AttoLT.Result [Tag]
run = AttoLT.parse html