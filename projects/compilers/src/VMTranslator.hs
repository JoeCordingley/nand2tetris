module VMTranslator (translateFile) where

import Lib
import Parser
import Data.List (intercalate)
import Control.Applicative (many, (<|>))
import Data.Functor (void)

translateFile :: InputFile -> OutputFile -> IO ()
translateFile = compileFile translate

translate :: String -> Maybe String
translate = fmap (intercalate "\n") . runParser (fmap concat $ many line)

line :: Parser [String]
line = fmap concat (optional codePortion) <* optional comment <* endOfLine

comment :: Parser ()
comment = void $ string "//" *> many (ifChar (/= '\n'))

codePortion :: Parser [String]
codePortion = push <|> command

push :: Parser [String]
push = token (string "push") *> token segment <*> index where
  index = int 
  segment = push' <$ string "constant" 

push' :: Int -> [String]
push' i = [ainstruction i, "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1"] where
  ainstruction i' = "@" <> show i'

command :: Parser [String]
command = add where
  add = ["@SP", "M=M-1", "A=M", "D=M", "@SP", "M=M-1", "A=M", "M=D+M", "@SP", "M=M+1"] <$ string "add"

