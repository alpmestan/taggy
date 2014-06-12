{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Text.Taggy        as Taggy
import qualified Text.HTML.TagSoup as Tagsoup
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V

sizes :: [Int]
sizes = [50, 500, 5000]

sizes' :: [Int]
sizes' = [50000, 500000, 5000000]

benchOnFile :: String -> FilePath -> Benchmark
benchOnFile label fp =
  bgroup label
    [ bench "tagsoup"        $ whnfIO (tagsoup fp) 
    , bench "taggy"          $ whnfIO (taggy   fp)
    , bench "taggy-entities" $ whnfIO (taggyEntities fp)
    ]

tagsoup :: FilePath -> IO (V.Vector (Tagsoup.Tag T.Text))
tagsoup fp = T.readFile ("html_files/" ++ fp) 
         >>= return . V.fromList . Tagsoup.parseTags

taggy :: FilePath -> IO (V.Vector Taggy.Tag)
taggy fp = T.readFile ("html_files/" ++ fp) 
       >>= return . V.fromList . Taggy.taggyWith False

taggyEntities :: FilePath -> IO (V.Vector Taggy.Tag)
taggyEntities fp = T.readFile ("html_files/" ++ fp)
               >>= return . V.fromList . Taggy.taggyWith True

linkBench :: Int -> Benchmark
linkBench size = benchOnFile (show size) fp

  where fp = "links_" ++ show size ++ ".html"

main :: IO ()
main = 
  defaultMain
    [ benchOnFile "alpmestan.com index"
                  "alpmestan.html"

    , benchOnFile "google search"
                  "googling-haskell.html"
    
    , benchOnFile "youtube"
                  "youtube.html"
    
    , benchOnFile "wikipedia - history of mathematics" 
                  "wikipedia_history_of_mathematics.html"

    , benchOnFile "worldslongestwebsite.com"
                  "worldslongestwebsite.com.html"

    , benchOnFile "/r/haskell"
                  "haskell_reddit.html"

    -- , bgroup "links" $ map linkBench sizes
    ]
