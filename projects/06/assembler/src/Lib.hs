module Lib
    ( printFile
    , InputFile(..)
    , OutputFile(..)
    , assemble
    , line
    ) where

import System.IO (withFile, IOMode(ReadMode, WriteMode), hGetContents, hPutStrLn)
import Text.Read (readMaybe) 
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Control.Monad.State.Lazy (evalStateT)
import Control.Applicative (many, empty, (<|>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Parser

data InputFile = InputFile String
data OutputFile = OutputFile String

printFile :: InputFile -> OutputFile -> IO ()
printFile (InputFile input) (OutputFile output) = 
  withFile input ReadMode $ \i -> 
    withFile output WriteMode $ \o -> do
      contents <- hGetContents i 
      case assemble contents of
        Just assembled -> hPutStrLn o assembled *> putStrLn "success"
        Nothing -> putStrLn "failure"
      
assemble :: String -> Maybe String
assemble = headMay . evalStateT (intercalate "\n" <$> many line <* eof) where
  headMay (a:_) = pure a
  headMay [] = empty

line :: Parser String
line = instruction <* endOfLine

instruction :: Parser String
instruction = ainstruction <|> cinstruction
  
ainstruction :: Parser String
ainstruction = do
  v <- char '@' *> many digit
  case binaryValue v of
    Nothing -> empty
    Just a -> return $ "0" <> a 

binaryValue :: String -> Maybe String
binaryValue s = readMaybe s >>= pad .  toBin where
  pad b = case 15 - (length b) of
    n | n >= 0 -> Just $ replicate n '0' <> b
      | otherwise -> Nothing

toBin :: Int -> String
toBin d = showIntAtBase 2 intToDigit d ""

cinstruction :: Parser String
cinstruction = do
  d <- dest <* char '=' <|> return "000"
  c <- comp
  j <- char ';' *> jump <|> return "000"
  return $ "111" <> c <> d <> j 

dest :: Parser String 
dest = foldr f empty
  [ ("001", "M")
  , ("010", "D") 
  , ("011", "MD") 
  , ("100", "A") 
  , ("101", "AM")
  , ("110", "AD")
  , ("111", "AMD")
  ] where f (code, symbol) rest = code <$ string symbol <|> rest

comp :: Parser String 
comp = dComp <|> mComp where
  dComp = fmap ('0':) $ comp' "A"
  mComp = fmap ('1':) $ comp' "M"
  comp' aOrM = foldr1 (<|>) . fmap f $ ("101010", "0") :|
    [ ("111111", "1")
    , ("111010", "-1")
    , ("001100", "D") 
    , ("110000", aOrM)
    , ("001101", "!" <> "D") 
    , ("110001", "!" <> aOrM) 
    , ("001111", "-" <> "D") 
    , ("110011", "-" <> aOrM)
    , ("011111", "D" <> "+1")
    , ("110111", aOrM <> "+1")
    , ("001110", "D" <> "-1")
    , ("110010", aOrM <> "-1")
    , ("000010", "D+" <> aOrM) 
    , ("010011", "D-" <> aOrM) 
    , ("000111", aOrM <> "-D") 
    , ("000000", "D&" <> aOrM) 
    , ("010101", "D|" <> aOrM) 
    ] where f (code, symbol) = code <$ string symbol 

jump :: Parser String 
jump = foldr f empty
  [ ("001", "JGT")
  , ("010", "JEQ") 
  , ("011", "JGE") 
  , ("100", "JLT") 
  , ("101", "JNE") 
  , ("110", "JLE") 
  , ("111", "JMP") 
  ] where f (code, symbol) rest = code <$ string symbol <|> rest

