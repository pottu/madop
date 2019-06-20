module Main where

import System.Environment (getArgs)
import Parser 

main :: IO ()
main = do
    -- TODO: Handle multiple/invalid args"
    args <- getArgs
    if null args
    then putStrLn "Please supply string to be parsed."
    else do
        mdToParse <- getArgs
        print $ parseMd $ head mdToParse
