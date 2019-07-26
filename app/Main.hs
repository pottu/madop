module Main where

import System.IO (readFile, getContents) 
import System.Environment (getArgs)
import Parser (parseMd)
import Renderer (renderHTML)
import Text.Parsec as Prsc
import Text.Parsec.String (Parser)

type Input = IO String
type Output = String -> IO ()

data ExecMode = Default Input Output 
              | Help
              | Version

currVersion = "0.X"

usageMsg = "A simple Markdown to HTML converter.\n\
           \Usage: madop [-o output_file] (--stdin | input_file)\n\
           \       madop [-h|-v]\n\n\
           \Options:\n\
           \  -v | --version    Print installed madop version\n\
           \  -h | --help       Print this message\n\
           \  -o | -- output <file>\n\
           \                    Place the output into <file>\n\
           \  --stdin           Get input from stdin instead of a file"



-- FIXME: Better error msg when trouble opening file.
main :: IO ()
main = do
  args <- getArgs
  let parsed = Prsc.parse parseArgs "" (unwords args)
   in either (error . show) execute parsed
  where
    execute (Default input output) = do
      contents <- input 
      output $ renderHTML $ parseMd contents

    execute Help = putStrLn usageMsg

    execute Version = putStrLn $ "madop version " ++ currVersion


parseArgs :: Parser ExecMode
parseArgs =  Prsc.try help
        <|> Prsc.try version
        <|> pDefault
        <|> return Help



pDefault :: Parser ExecMode
pDefault = do
  output <- (Prsc.try fileOutput) <|> return putStr
  input <- (Prsc.try stdInput) <|> fileInput
  return $ Default input output 
  where
    fileOutput :: Parser Output
    fileOutput = do
      Prsc.try (Prsc.string "-o") <|> Prsc.string "--output"
      Prsc.many1 $ Prsc.char ' '
      outfile <- Prsc.manyTill Prsc.anyChar (Prsc.char ' ')
      return $ writeFile outfile

    stdInput :: Parser Input 
    stdInput = do
      Prsc.string "--stdin"
      return getContents 
      
    fileInput :: Parser Input 
    fileInput = do
      file <- Prsc.many1 Prsc.anyChar 
      return $ readFile file
  


help :: Parser ExecMode
help = do
  Prsc.try (Prsc.string "-h") <|> Prsc.string "--help"
  return Help

version :: Parser ExecMode
version = do
  Prsc.try (Prsc.string "-v") <|> Prsc.string "--version"
  return Version

