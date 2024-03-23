{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import VMTranslator
import Options.Applicative
import Text.Regex.TDFA
import Lib

main :: IO ()
main = (uncurry translateFile) =<< execParser opts
  where
    opts = info (fileParser <**> helper)
      ( fullDesc
     <> progDesc "translate from vm to hack assembly"
     <> header "haskell nand2Tetris vm translator" )

fileParser :: Parser (InputFile, OutputFile)
fileParser = argument (eitherReader parseFileName) (metavar "FILE.vm")

vmRegex :: String
vmRegex = "^(.*)\\.vm$"

parseFileName :: String -> Either String (InputFile, OutputFile) 
parseFileName s = case s =~ vmRegex of 
  ((_, "", _, _) :: (String, String, String, [String])) -> Left "invalid filename"
  ((_, m, _, matches) :: (String, String, String, [String])) -> Right $ (InputFile m, OutputFile (head matches <> ".asm"))
