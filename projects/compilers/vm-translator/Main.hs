{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Options.Applicative
import Text.Regex.TDFA
import VMTranslator (translateInnerFunctionFile)

main :: IO ()
main = translateFile =<< execParser opts
  where
    translateFile (filePrefix, inputFile, outputFile, Just functionName) = translateInnerFunctionFile functionName filePrefix inputFile outputFile
    opts =
        info
            (argsParser <**> helper)
            ( fullDesc
                <> progDesc "translate from vm to hack assembly"
                <> header "haskell nand2Tetris vm translator"
            )

type Args = (FilePrefix, InputFile, OutputFile, Maybe String)

argsParser :: Parser Args
argsParser = f <$> fileParser <*> optional function
  where
    f (filePrefix, inputFile, outputFile) functionName = (filePrefix, inputFile, outputFile, functionName)

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

function :: Parser String
function = strOption (long "function" <> short 'f' <> metavar "functionName")
