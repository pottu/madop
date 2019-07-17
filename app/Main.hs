module Main where

import System.Environment (getArgs)
import Parser (parseMd)
import Renderer (renderHTML)
import Text.Parsec as Prsc
import Text.Parsec.String (Parser)
import System.IO as IO

type Input = IO String

data Exec = Default Input 
          | Output String Input 
          | Help

usageMsg = "A simple Markdown to HTML converter.\n\
           \Usage: madop [-o output_file] (--stdin | input_file)\n\
           \       madop [-h|--help]\n\n\
           \Options:\n\
           \  -h | --help  Display this message\n\
           \  -o <file>    Place the output into <file>\n\
           \  --stdin      Get input from stdin instead of a file"

-- TODO: Handle multiple/invalid args
-- FIXME: Better error msg when trouble opening file.
main :: IO ()
main = do
  args <- getArgs
  let parsed = Prsc.parse parseArgs "" (unwords args)
   in either (error . show) execute parsed
  where
    execute (Default input) = do
      contents <- input 
      putStr $ renderHTML $ parseMd contents

    execute (Output outfile input) = do
      contents <- input 
      writeFile outfile (renderHTML (parseMd contents))

    execute Help = putStrLn usageMsg


parseArgs :: Parser Exec 
parseArgs = Prsc.try output
        <|> Prsc.try help
        <|> pDefault
        <|> return Help

pDefault :: Parser Exec
pDefault = do
  input <- (Prsc.try stdinInput) <|> fileInput
  return $ Default input 
  

output :: Parser Exec
output = do
  Prsc.string "-o"
  Prsc.many1 $ Prsc.char ' '
  outfile <- Prsc.manyTill Prsc.anyChar (Prsc.char ' ')
  input <- (Prsc.try stdinInput) <|> fileInput
  return $ Output outfile input 

help :: Parser Exec
help = do
  Prsc.try (Prsc.string "-h") <|> Prsc.string "--help"
  return Help

stdinInput :: Parser Input 
stdinInput = do
  Prsc.string "--stdin"
  return IO.getContents 
  
fileInput :: Parser Input 
fileInput = do
  file <- Prsc.many1 Prsc.anyChar 
  return $ IO.readFile file




