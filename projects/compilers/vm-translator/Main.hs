{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Options.Applicative
import Text.Regex.TDFA
import VMTranslator

main :: IO ()
main = translateFile' =<< execParser opts
  where
    translateFile' (filePrefix, inputFile, outputFile) = translateFile filePrefix inputFile outputFile
    opts =
        info
            (fileParser <**> helper)
            ( fullDesc
                <> progDesc "translate from vm to hack assembly"
                <> header "haskell nand2Tetris vm translator"
            )

fileParser :: Parser (FilePrefix, InputFile, OutputFile)
fileParser = argument (eitherReader parseFileName) (metavar "FILE.vm")

vmRegex :: String
vmRegex = "^(.*)\\.vm$"

parseFileName :: String -> Either String (FilePrefix, InputFile, OutputFile)
parseFileName s = case s =~ vmRegex of
    ((_, "", _, _) :: (String, String, String, [String])) -> Left "invalid filename"
    ((_, m, _, matches) :: (String, String, String, [String])) -> Right $ (FilePrefix filePrefix, InputFile m, OutputFile (filePrefix <> ".asm"))
      where
        filePrefix = head matches
