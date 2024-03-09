module Lib
    ( printFile
    , InputFile(..)
    , OutputFile(..)
    , assemble
    ) where

import System.IO (withFile, IOMode(ReadMode, WriteMode), hGetContents, hPutStrLn)
import Text.Read (readMaybe) 
import Data.Map (Map, unionWith)
import Numeric (showIntAtBase)
import Data.Char (intToDigit, isDigit)
import Control.Monad.State.Lazy (StateT(..), evalStateT)
import Control.Applicative (Alternative, many, empty, (<|>))
import Data.List (uncons)

data InputFile = InputFile String
data OutputFile = OutputFile String

type Parser = StateT String []

printFile :: InputFile -> OutputFile -> IO ()
printFile (InputFile input) (OutputFile output) = 
  withFile input ReadMode $ \i -> 
    withFile output WriteMode $ \o -> do
      contents <- hGetContents i 
      case assemble contents of
        Just assembled -> hPutStrLn o assembled *> putStrLn "success"
        Nothing -> putStrLn "failure"
      
assemble :: String -> Maybe String
assemble = headMay . evalStateT (concat <$> many line) where
  headMay (a:_) = Just a
  headMay [] = Nothing

line :: Parser String
line = instruction <* char '\n'

anyChar :: Parser Char
anyChar = StateT $ toNE . uncons where
  toNE (Just (a,as)) = pure (a, as)
  toNE Nothing = fail "eof"


ifChar :: (Char -> Bool) -> Parser Char
ifChar p = anyChar >>= guarded p 

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

char :: Char -> Parser Char
char = ifChar . (==)

string :: String -> Parser String
string = traverse char

instruction :: Parser String
instruction = fmap (<>"\n") (ainstruction <|> cinstruction)
  
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
dest = 
  "001" <$ char 'M' <|>
  "010" <$ char 'D'

comp :: Parser String 
comp = dComp <|> mComp where
  dComp = fmap ('0':) $ comp' 'D'
  mComp = fmap ('1':) $ comp' 'M'
  comp' dOrM = 
    "000010" <$ string (dOrM:"+A") <|>
    "001100" <$ char dOrM <|>
    "110000" <$ char 'A' 

jump :: Parser String 
jump = "001" <$ string "JGT"

data MonoidMap a b = MonoidMap (Map a b) deriving Show

instance (Ord a, Semigroup b) => Semigroup (MonoidMap a b) where
  MonoidMap l <> MonoidMap r = MonoidMap $ unionWith (<>) l r

instance (Ord a, Semigroup b) => Monoid (MonoidMap a b) where
  mempty = MonoidMap mempty

digit :: Parser Char
digit = ifChar isDigit

