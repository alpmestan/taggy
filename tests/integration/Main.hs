{-# LANGUAGE TupleSections #-}

module Main (main) where

import Prelude hiding (readFile)
import Data.Functor ((<$>))
import Data.List (isSuffixOf)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (readFile)
import Paths_taggy (getDataFileName)
import System.Directory (getDirectoryContents, setCurrentDirectory)
import Test.Hspec (hspec, runIO, describe, it, shouldSatisfy)
import Text.Taggy (taggyWith)

getHTMLFiles :: IO [(FilePath, Text)]
getHTMLFiles = getDataFileName "html_files" 
           >>= setCurrentDirectory 
            >> filter (isSuffixOf ".html") <$> getDirectoryContents "."  
           >>= mapM (\name -> fmap (name,) $ readFile name)

main :: IO ()
main = hspec . (runIO getHTMLFiles >>=) . mapM_ $ \(name, content) ->
  describe name . it "Should parse without error." $
    taggyWith True content `shouldSatisfy` not . null
