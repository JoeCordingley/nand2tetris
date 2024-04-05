{-# LANGUAGE FlexibleContexts #-}

module Parser (
    endOfLine,
    char,
    string,
    digit,
    eof,
    anyChar,
    optional,
    ifChar,
    charIn,
    runParser,
    Parser,
    int,
    token,
) where

import Control.Applicative (Alternative, empty, many, (<|>))
import Control.Monad.State.Lazy (MonadState (get, put), StateT (..), evalStateT)
import Data.Char (isDigit)
import Data.Functor (void)
import Data.List (uncons)
import Lib
import Text.Read (readMaybe)

type Parser = StateT (Maybe String) []

endOfLine :: (MonadState (Maybe String) m, Alternative m) => m ()
endOfLine = void (string "\r\n") <|> void (char '\n') <|> eof

unfinishedSource :: (MonadState (Maybe String) m, Alternative m) => m String
unfinishedSource = get >>= liftMaybe

eof :: (MonadState (Maybe String) m, Alternative m) => m ()
eof = do
    s <- unfinishedSource
    case s of
        "" -> put Nothing
        _ -> empty

string :: (MonadState (Maybe String) m, Alternative m) => String -> m String
string = traverse char

char :: (MonadState (Maybe String) m, Alternative m) => Char -> m Char
char = ifChar . (==)

ifChar :: (MonadState (Maybe String) m, Alternative m) => (Char -> Bool) -> m Char
ifChar p = anyChar >>= guarded p

charIn :: (MonadState (Maybe String) m, Alternative m) => [Char] -> m Char
charIn chars = ifChar $ flip elem chars

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

anyChar :: (MonadState (Maybe String) m, Alternative m) => m Char
anyChar = do
    s <- unfinishedSource
    (a, rest) <- liftMaybe $ uncons s
    a <$ put (Just rest)

digit :: (MonadState (Maybe String) m, Alternative m) => m Char
digit = ifChar isDigit

optional :: (Alternative m) => m a -> m (Maybe a)
optional p = fmap Just p <|> pure Nothing

runParser :: Parser a -> String -> Maybe a
runParser p = headMay . evalStateT p . Just

int :: (MonadState (Maybe String) m, Alternative m) => m Int
int = do
    digits <- many digit
    liftMaybe $ readMaybe digits

token :: (MonadState (Maybe String) m, Alternative m) => m a -> m a
token p = p <* whitespace

whitespace :: (MonadState (Maybe String) m, Alternative m) => m ()
whitespace = void $ many $ char ' '

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
    fmap f (Cont ff) = Cont ff'
      where
        ff' g = ff $ g . f

instance Applicative (Cont r) where
    Cont ff <*> Cont fa = Cont b
      where
        b g = ff h
          where
            h i = fa (g . i)
    pure a = Cont ($ a)

instance Monad (Cont r) where
    Cont fa >>= f = Cont g
      where
        g h = fa i
          where
            i a = (runCont . f) a h
