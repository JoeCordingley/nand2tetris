{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Options.Applicative
import Text.Regex.TDFA
import VMTranslator (translateDir, translateFunctionFile, translateInnerFunctionFile, vmRegex)

main :: IO ()
main = run =<< execParser opts
  where
    run (F (filePrefix, inputFile, outputFile, maybeFunctionName)) = f filePrefix inputFile outputFile
      where
        f = case maybeFunctionName of
            (Just functionName) -> translateInnerFunctionFile functionName
            Nothing -> translateFunctionFile
    run (D dir) = translateDir dir
    opts =
        info
            (subCommandParser <**> helper)
            ( fullDesc
                <> progDesc "translate from vm to hack assembly"
                <> header "haskell nand2Tetris vm translator"
            )
      where
        subCommandParser =
            subparser $
                command "file" (info (F <$> argsParser <**> helper) (progDesc "translate file"))
                    <> command "dir" (info (D <$> dirParser <**> helper) (progDesc "translate directory"))

data Args = F FileArgs | D Directory
type FileArgs = (FilePrefix, InputFile, OutputFile, Maybe String)

dirParser :: Parser Directory
dirParser = argument (fmap Directory str) (metavar "DIR")

argsParser :: Parser FileArgs
argsParser = f <$> fileParser <*> optional function
  where
    f (filePrefix, inputFile, outputFile) functionName = (filePrefix, inputFile, outputFile, functionName)

fileParser :: Parser (FilePrefix, InputFile, OutputFile)
fileParser = argument (eitherReader parseFileName) (metavar "FILE.vm")

parseFileName :: String -> Either String (FilePrefix, InputFile, OutputFile)
parseFileName s = case s =~ vmRegex of
    ((_, "", _, _) :: (String, String, String, [String])) -> Left "invalid filename"
    ((_, m, _, path : prefix : _) :: (String, String, String, [String])) -> Right (FilePrefix prefix, InputFile m, OutputFile (path <> ".asm"))
    _ -> undefined

function :: Parser String
function = strOption (long "function" <> short 'f' <> metavar "functionName")
