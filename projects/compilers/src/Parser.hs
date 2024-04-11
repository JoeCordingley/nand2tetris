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
    lexeme,
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

lexeme :: (MonadState (Maybe String) m, Alternative m) => m a -> m a
lexeme p = p <* whitespace

whitespace :: (MonadState (Maybe String) m, Alternative m) => m ()
whitespace = void $ many $ char ' '
