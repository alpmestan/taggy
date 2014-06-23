taggy
=====

An attoparsec based html parser. [![Build Status](https://secure.travis-ci.org/alpmestan/taggy.png?branch=master)](http://travis-ci.org/alpmestan/taggy)

Currently very WIP but already supports a fairly decent range of common websites. I haven't managed to find a website with which it chokes, using the current parser. The performance is quite promising.

Using `taggy`
=============

_taggy_ has a `taggyWith` function to work on HTML à la _tagsoup_.

``` haskell
taggyWith :: Bool -> LT.Text -> [Tag]
```

The `Bool` there just lets you specify whether you want to convert the special HTML entities to their corresponding unicode character. `True` means "yes convert them please". This function takes lazy `Text` as input.

Or you can use the raw `run` function, which returns a good old `Result` from _attoparsec_.

``` haskell
run :: Bool -> LT.Text -> AttoLT.Result [Tag]
```

For example, if you want to read the html code from a file, and print one tag per line, you could do:

``` haskell
import Data.Attoparsec.Text.Lazy (eitherResult)
import qualified Data.Text.Lazy.IO as T
import Text.Taggy (run)

taggy :: FilePath -> IO ()
taggy fp = do
  content <- T.readFile fp
  either (\s -> putStrLn $ "couldn't parse: " ++ s) 
         (mapM_ print) 
         (eitherResult $ run True content)
```

But _taggy_ also started providing support for DOM-syle documents. This is computed from the list of tags gained by using `taggyWith`.

If you fire up ghci with _taggy_ loaded:

``` bash
$ cabal repl # if working with a copy of this repo
```

You can see this `domify` in action.

``` haskell
λ> :set -XOverloadedStrings
λ> head . domify . taggyWith False $ "<html><head></head><body>yo</body></html>"
NodeElement (Element {eltName = "html", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "head", eltAttrs = fromList [], eltChildren = []}),NodeElement (Element {eltName = "body", eltAttrs = fromList [], eltChildren = [NodeContent "yo"]})]})
```

I'm already working on a companion `taggy-lens` library.
