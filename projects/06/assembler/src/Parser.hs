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
  ) where

import Control.Monad.State.Lazy (StateT(..))
import Control.Monad (void, guard)
import Data.List (uncons, singleton)
import Control.Applicative (Alternative, (<|>), empty)
import Data.Char (isDigit)
import Data.Map (Map, unionWith)
import Data.Maybe (maybeToList, isNothing)

type Parser = StateT (Maybe String) []

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> eof

eof :: Parser ()
eof = StateT $ f where
  f (Just "") = pure ((), Nothing)
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
anyChar = StateT $ f where
  f maybeString = maybeToList $ do
    s <- maybeString
    (c, rest) <- uncons s
    return (c, Just rest)

digit :: Parser Char
digit = ifChar isDigit

optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> pure Nothing

runParser :: Parser a -> String -> [a]
runParser p s = do
  (a, m) <- runStateT p (Just s)
  guard (isNothing m)
  return a

data MonoidMap a b = MonoidMap (Map a b) deriving Show

instance (Ord a, Semigroup b) => Semigroup (MonoidMap a b) where
  MonoidMap l <> MonoidMap r = MonoidMap $ unionWith (<>) l r

instance (Ord a, Semigroup b) => Monoid (MonoidMap a b) where
  mempty = MonoidMap mempty

data ListT m a = ListT {runListT :: m [a]}

instance Functor f => Functor (ListT f) where
  fmap f (ListT mas) = ListT $ (fmap . map) f mas

instance Applicative f => Applicative (ListT f) where
  ListT ffs <*> ListT fas = ListT $ (<*>) <$> ffs <*> fas 
  pure = ListT . pure . singleton
    
instance Monad m => Monad (ListT m) where
  ListT mas >>= f = ListT $ mas >>= fmap concat . traverse (runListT . f) 
      
