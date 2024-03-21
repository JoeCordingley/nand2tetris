{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Options.Applicative
import Text.Regex.TDFA

main :: IO ()
main = (uncurry printFile) =<< execParser opts
  where
    opts = info (fileParser <**> helper)
      ( fullDesc
     <> progDesc "Assemble a hack asm"
     <> header "haskell nand2Tetris hack assembler" )

fileParser :: Parser (InputFile, OutputFile)
fileParser = argument (eitherReader parseFileName) (metavar "FILE.asm")

asmRegex :: String
asmRegex = "^(.*)\\.asm$"

parseFileName :: String -> Either String (InputFile, OutputFile) 
parseFileName s = case s =~ asmRegex of 
  ((_, "", _, _) :: (String, String, String, [String])) -> Left "invalid filename"
  ((_, m, _, matches) :: (String, String, String, [String])) -> Right $ (InputFile m, OutputFile (head matches <> ".hack"))
