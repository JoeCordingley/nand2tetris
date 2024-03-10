module Parser(Parser, endOfLine, char, string, digit, eof) where

import Control.Monad.State.Lazy (StateT(..))
import Control.Monad (void)
import Data.List (uncons)
import Control.Applicative (Alternative, (<|>), empty)
import Data.Char (isDigit)
import Data.Map (Map, unionWith)

type Parser = StateT String []

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> eof

eof :: Parser ()
eof = StateT $ f . uncons where
  f Nothing = pure ((), "")
  f _ = empty

string :: String -> Parser String
string = traverse char

char :: Char -> Parser Char
char = ifChar . (==)

ifChar :: (Char -> Bool) -> Parser Char
ifChar p = anyChar >>= guarded p 

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

anyChar :: Parser Char
anyChar = StateT $ toNE . uncons where
  toNE (Just t) = pure t
  toNE Nothing = empty

digit :: Parser Char
digit = ifChar isDigit

data MonoidMap a b = MonoidMap (Map a b) deriving Show

instance (Ord a, Semigroup b) => Semigroup (MonoidMap a b) where
  MonoidMap l <> MonoidMap r = MonoidMap $ unionWith (<>) l r

instance (Ord a, Semigroup b) => Monoid (MonoidMap a b) where
  mempty = MonoidMap mempty
