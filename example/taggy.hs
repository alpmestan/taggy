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
    [opt, filename] | opt == "-d" || opt == "--dom" -> dom filename
    [opt, filename] | opt == "-t" || opt == "--t" -> taggy filename
    [filename] -> taggy filename
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "taggy - simple and fast HTML parser"
  putStrLn ""
  putStrLn "Usage:\t taggy [OPTION] <HTML file name>"
  putStrLn "\n"
  putStrLn "Options are:"
  putStrLn "\t -d/--dom\t Parse as a DOM tree. This isn't the default."
  putStrLn "\t -t/--tags\t Parse as a list of opening/closing/text/comment/script/style tags"

taggy :: FilePath -> IO ()
taggy fp = do
  content <- T.readFile fp
  either (\s -> putStrLn $ "couldn't parse: " ++ s) 
         (mapM_ print) 
         (eitherResult $ run content)

dom :: FilePath -> IO ()
dom fp = do
  content <- T.readFile fp
  mapM_ print . g' $ tagsIn content