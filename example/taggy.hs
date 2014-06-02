module Main where

import Data.Attoparsec.Text.Lazy (eitherResult)
import System.Environment
import Text.Taggy

import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> taggy (head args)
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "taggy - simple and fast HTML parser"
  putStrLn ""
  putStrLn "Usage:\t taggy <HTML file name>"

taggy :: FilePath -> IO ()
taggy fp = do
  content <- T.readFile fp
  either (\s -> putStrLn $ "couldn't parse: " ++ s) 
         (mapM_ print) 
         (eitherResult $ run content)
