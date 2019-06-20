module Main where

import System.Environment (getArgs)
import Parser 
import Renderer

main :: IO ()
main = do
    -- TODO: Handle multiple/invalid args"
    args <- getArgs
    if null args
    then putStrLn "Please supply string to be parsed."
    else do
        mdToParse <- getArgs
        putStrLn $ renderHTML $ parseMd $ head mdToParse
