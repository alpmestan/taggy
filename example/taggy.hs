module Main where

import Data.Attoparsec.Text.Lazy (eitherResult)
import System.Environment
import Text.Taggy

import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    (opt : filenames) | opt == "-d" || opt == "--dom" -> mapM_ dom filenames
    (opt : filenames) | opt == "-t" || opt == "--t" -> mapM_ taggy filenames
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "taggy - simple and fast HTML parser"
  putStrLn ""
  putStrLn "Usage:\t taggy <format> file1.html file2.html file3.html ..."
  putStrLn "\n"
  putStrLn "Formats are:"
  putStrLn "\t -d/--dom\t Parse as a DOM tree."
  putStrLn "\t -t/--tags\t Parse as a list of opening/closing/text/comment/script/style tags"

taggy :: FilePath -> IO ()
taggy fp = do
  content <- T.readFile fp
  either (\s -> putStrLn $ "couldn't parse: " ++ s) 
         (mapM_ print) 
         (eitherResult $ run True content)

dom :: FilePath -> IO ()
dom fp = do
  content <- T.readFile fp
  mapM_ print . domify $ taggyWith True content
