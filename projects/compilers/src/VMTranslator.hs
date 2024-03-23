module VMTranslator (translateFile) where

import Lib
import Parser
import Data.List (intercalate)
import Control.Applicative (many, (<|>))

translateFile :: InputFile -> OutputFile -> IO ()
translateFile = compileFile translate

translate :: String -> Maybe String
translate = fmap (intercalate "\n") . runParser (fmap concat $ many line)

line :: Parser [String]
line = push <|> command

push :: Parser [String]
push = token (string "push") *> token segment <*> token index where
  index = int 
  segment = push' <$ string "constant" 

push' :: Int -> [String]
push' i = [ainstruction i, "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1"] where
  ainstruction i' = "@" <> show i'

command :: Parser [String]
command = add where
  add = ["@SP", "M=M-1", "A=M", "D=M", "@SP", "M=M-1", "A=M", "M=D+M", "@SP", "M=M+1"] <$ token (string "add")

