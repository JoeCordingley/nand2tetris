{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language RankNTypes #-}

module Parser
  ( Parser
  , endOfLine
  , char
  , string
  , digit
  , eof
  , anyChar
  , optional
  , runParser
  , ifChar
  , charIn
  , WithSource(..)
  ) where

import Control.Monad.State.Lazy (StateT(..), MonadState)
import Control.Monad (guard, MonadPlus)
import Data.List (uncons)
import Control.Applicative (Alternative, (<|>), empty)
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Data.Functor (void)
import Control.Lens (Lens', use, (.=))

class WithSource s where
  sourceLens :: Lens' s (Maybe String)

instance WithSource (Maybe String) where
  sourceLens = id

type Parser = StateT (Maybe String) []

endOfLine :: (MonadState (Maybe String) m, MonadPlus m) => m ()
endOfLine = void (char '\n') <|> eof

eof :: (MonadState s m, MonadPlus m, WithSource s) => m ()
eof = do
  s <- use sourceLens
  case s of
    Just "" -> sourceLens .= Nothing
    _ -> empty

string :: (MonadState s m, MonadPlus m, WithSource s) => String -> m String
string = traverse char

char :: (MonadState s m, MonadPlus m, WithSource s) => Char -> m Char
char = ifChar . (==)

ifChar :: (MonadState s m, MonadPlus m, WithSource s) => (Char -> Bool) -> m Char
ifChar p = anyChar >>= guarded p 

charIn :: (MonadState s m, MonadPlus m, WithSource s) => [Char] -> m Char
charIn chars = ifChar $ flip elem chars

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

anyChar :: (MonadState s m, MonadPlus m, WithSource s) => m Char
anyChar = do
  maybeString <- use sourceLens
  (c, rest) <- liftMaybe $ maybeString >>= uncons
  sourceLens .= Just rest
  return c

liftMaybe :: Alternative f => Maybe a -> f a
liftMaybe Nothing = empty
liftMaybe (Just a) = pure a

digit :: (MonadState s m, MonadPlus m, WithSource s) => m Char
digit = ifChar isDigit

optional :: MonadPlus m => m a -> m (Maybe a)
optional p = fmap Just p <|> pure Nothing

runParser :: Parser a -> String -> [a]
runParser p s = do
  (a, m) <- runStateT p (Just s)
  guard (isNothing m)
  return a

      
