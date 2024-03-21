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
  , Parser
  ) where

import Control.Monad.State.Lazy (StateT(..))
import Data.List (uncons)
import Control.Applicative (Alternative, (<|>), empty)
import Data.Char (isDigit)
import Data.Functor (void)
import Data.Maybe (maybeToList)

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

