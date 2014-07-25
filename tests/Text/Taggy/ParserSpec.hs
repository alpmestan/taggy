{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.ParserSpec where

import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy
import Test.Hspec
import Test.Hspec.Attoparsec (shouldParse)
import qualified Test.Hspec.Attoparsec.Source (Source((~>)))
import Text.Taggy.Parser
import Text.Taggy.Types

spec :: Spec
spec = do
  describe "tagopen parser" $ do
    it "successfully parses <b>" $
      "<b>" ~> tagopen False
        `shouldParse` TagOpen "b" [] False

    it "successfully parses <a href=\"/home\">" $
      "<a href=\"/home\">" ~> tagopen False
        `shouldParse` TagOpen "a" [Attribute "href" "/home"] False

    it "successfully parses <button data-foo=\"bar\">" $ 
      "<a data-foo=\"bar\">" ~> tagopen False
        `shouldParse` TagOpen "a" [Attribute "data-foo" "bar"] False

    it "successfully (and forgivingly) parses <<br>" $
      "<<br>" ~> tagopen False
        `shouldParse` TagOpen "br" [] False

    it "successfully parses <elem.b>" $
      "<elem.b>" ~> tagopen False
        `shouldParse` TagOpen "elem.b" [] False

    it "successfully detects self-closing tags: <br/>" $
      "<br/>" ~> tagopen False
        `shouldParse` TagOpen "br" [] True

    it "successfully detects self-closing tags: <br />" $
      "<br />" ~> tagopen False
        `shouldParse` TagOpen "br" [] True

    it "successfully detects self-closing tags: <br / >" $
      "<br / >" ~> tagopen False
        `shouldParse` TagOpen "br" [] True

    it "successfully (and forgivingly) parses <title foo\">" $
      "<title foo\">" ~> tagopen False
        `shouldParse` TagOpen "title" [Attribute "foo\"" ""] False
    it "successfully (and forgivingly) parses <title https://github.com/alpmestan/taggy\">" $
      "<title https://github.com/alpmestan/taggy/\">" ~> tagopen False
        `shouldParse` TagOpen "title" [Attribute "https://github.com/alpmestan/taggy/\"" ""] False

    it "can successfully convert entities in attribute values: <a title=\"&nbsp;Hello!\">" $
      "<a title=\"&nbsp;Hello!\">" ~> tagopen True
        `shouldParse` TagOpen "a" [Attribute "title" "\160Hello!"] False

  describe "text parser" $ do
    it "can successfully convert entities inside the content: foo &nbsp; hi &eacute; &me bar" $
      "foo &nbsp; hi &eacute; &me bar" ~> tagtext True
        `shouldParse` TagText "foo \160 hi \233 &me bar"

  describe "comment parser" $ do
    it "successfully parses <!-- foo -->" $
      "<!-- foo -->" ~> tagcomment
        `shouldParse` TagComment " foo "

    it "successfully parses <!--foo-->" $ 
      "<!--foo-->" ~> tagcomment
        `shouldParse` TagComment "foo"

  describe "script parser" $ do
    it "successfully parses a script section" $ 
      "<script type=\"text/javascript\">var x = 5;</script>" ~> tagscript False
        `shouldParse` TagScript (TagOpen "script" [Attribute "type" "text/javascript"] False)
                                "var x = 5;"
                                (TagClose "script")

    it "is not too dumb" $ 
      "<script>var str = '</script';</script>" ~> tagscript False
        `shouldParse` TagScript (TagOpen "script" [] False)
                                "var str = '</script';"
                                (TagClose "script")

  describe "style parser" $ do
    it "successfully parses a style section" $ 
      "<style type=\"text/css\">div { color: blue; }</style>" ~> tagstyle False
        `shouldParse` TagStyle (TagOpen "style" [Attribute "type" "text/css"] False)
                               "div { color: blue; }"
                               (TagClose "style")

  describe "(global) html parser" $ do
    it "successfull parses: <html><head><title>Hello</title></head><body><p>Hi there!</p><br /></body></html>" $
      "<html><head><title>Hello</title></head><body><p>Hi there!</p><br /></body></html>" ~> htmlWith False
        `shouldParse` [ TagOpen "html" [] False 
                      , TagOpen "head" [] False 
                      , TagOpen "title" [] False
                      , TagText "Hello"
                      , TagClose "title"
                      , TagClose "head"
                      , TagOpen "body" [] False
                      , TagOpen "p" [] False
                      , TagText "Hi there!"
                      , TagClose "p"
                      , TagOpen "br" [] True
                      , TagClose "body"
                      , TagClose "html"
                      ]
    it "doesn't choke on malformed closing tags" $ 
      "<strong>North Korea</strong" ~> htmlWith False
        `shouldParse` [ TagOpen "strong" [] False
                      , TagText "North Korea"
                      , TagClose "strong"
                      ]

    it "doesn't choke on trailing newlines" $
      "<html>foo</html>\n" ~> htmlWith False
        `shouldParse` [ TagOpen "html" [] False
                      , TagText "foo"
                      , TagClose "html"
                      , TagText "\n"
                      ]

(~>) :: Text -> Parser a -> Either String a
(~>) = (Test.Hspec.Attoparsec.Source.~>)
