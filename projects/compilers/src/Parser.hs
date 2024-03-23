module Parser
  ( endOfLine
  , char
  , string
  , digit
  , eof
  , anyChar
  , optional
  , ifChar
  , charIn
  , runParser
  , Parser
  , int
  , token
  ) where

import Control.Monad.State.Lazy (StateT(..), evalStateT)
import Data.List (uncons)
import Control.Applicative (Alternative, (<|>), empty, many, some)
import Data.Char (isDigit)
import Data.Functor (void)
import Data.Maybe (maybeToList)
import Lib
import Text.Read (readMaybe) 

type Parser = StateT (Maybe String) []

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> eof

eof :: Parser ()
eof = StateT $ maybeToList . f where
  f m = do
    s <- m
    case s of
      "" -> Just ((), Nothing)
      _  -> Nothing

string :: String -> Parser String
string = traverse char

char :: Char -> Parser Char
char = ifChar . (==)

ifChar :: (Char -> Bool) -> Parser Char
ifChar p = anyChar >>= guarded p 

charIn :: [Char] -> Parser Char
charIn chars = ifChar $ flip elem chars

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

anyChar :: Parser Char
anyChar = StateT $ maybeToList . f where
  f m = do
    s <- m
    (a, rest) <- uncons s
    return (a, Just rest)

digit :: Parser Char
digit = ifChar isDigit

optional :: Alternative m => m a -> m (Maybe a)
optional p = fmap Just p <|> pure Nothing

runParser :: Parser a -> String -> Maybe a
runParser p = headMay . evalStateT p . Just where
  headMay (a:_) = Just a
  headMay [] = Nothing

int :: Parser Int 
int = do 
  digits <- many digit
  liftMaybe $ readMaybe digits 

token :: Parser a -> Parser a
token p = p <* whitespace

whitespace :: Parser ()
whitespace = void $ some $ void (char ' ') <|> endOfLine
