module Main where

import System.Environment (getArgs)
import Parser 

main :: IO ()
main = do
    -- TODO: Handle multiple/invalid args
    mdToParse <- getArgs
    print $ parseMd $ head mdToParse
