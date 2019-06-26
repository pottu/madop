module Main where

import System.Environment (getArgs)
import Parser 
import Renderer

-- TODO: Handle multiple/invalid args
-- FIXME: Better error msg when trouble opening file.
main :: IO ()
main = do
  args <- getArgs
  if null args
     then
       putStrLn "Please supply a file to be parsed."
     else do
       file <- readFile $ head args
       putStrLn $ renderHTML $ parseMd file




