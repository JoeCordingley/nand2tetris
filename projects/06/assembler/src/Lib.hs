{-# Language FlexibleContexts #-}


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
import Control.Applicative (many, empty, some, (<|>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (maybeToList)
import Data.Functor (void)
import Data.Functor.Compose (Compose(..))
import Data.Function ((&))
import Control.Monad.State.Lazy (StateT, State, lift, modify, evalStateT, MonadState)
import Control.Monad (join, MonadPlus)
import Parser
import Control.Lens (Lens', (%=), use)
import Data.Map (insert, Map)

data InputFile = InputFile String
data OutputFile = OutputFile String

type CountingInstructions = StateT Int 

class WithInstructionCount s where
  instructionCountLens :: Lens' s Int

instance WithSource ParseState where
  sourceLens f (ParseState i s) = fmap (ParseState i) $ f s

instance WithInstructionCount ParseState where
  instructionCountLens f (ParseState i s) = fmap ( (&) s . ParseState) $ f i

data ParseState = ParseState Int (Maybe String)

printFile :: InputFile -> OutputFile -> IO ()
printFile (InputFile input) (OutputFile output) = 
  withFile input ReadMode $ \i -> 
    withFile output WriteMode $ \o -> do
      contents <- hGetContents i 
      case assemble $ contents of
        Just assembled -> hPutStrLn o assembled *> putStrLn "success"
        Nothing -> putStrLn "failure"

filterWhitespace :: String -> String
filterWhitespace = filter (/= ' ')
      
assemble :: String -> Maybe String
assemble = headMay . map (intercalate "\n") . runParser (evalStateT instructions 0) . filterWhitespace  where
  headMay (a:_) = pure a
  headMay [] = empty

instructions :: CountingInstructions Parser [String]
instructions = (>>= maybeToList) <$> many line 

line :: CountingInstructions Parser (Maybe String)
line = fmap join (optional codePortion) <* lift (optional comment) <* lift endOfLine 

comment :: Parser ()
comment = void $ string "//" *> many (ifChar (/= '\n'))

codePortion :: CountingInstructions Parser (Maybe String)
codePortion = Just <$> instruction

instruction :: CountingInstructions Parser String
instruction = lift (ainstruction <|> cinstruction)  <* modify (+1)

instruction2 :: (MonadState s m, MonadPlus m, WithSource s, WithInstructionCount s) => m String
instruction2 = (ainstruction <|> cinstruction)  <* (instructionCountLens %= (+1))

instructionCount :: (MonadState s m, MonadPlus m, WithInstructionCount s) => m Int
instructionCount = use instructionCountLens

label :: (MonadState s m, MonadPlus m, WithSource s, WithInstructionCount s, MonadState (Map String Int) n) => Compose m n ()
label = Compose $ do
  s <- char '(' *> symbol <* char ')'
  i <- instructionCount
  return $ modify $ insert s (i+1)

symbol :: (MonadState s m, MonadPlus m, WithSource s) => m String
symbol = (:) <$> charIn validFirstLetters <*> many (charIn validSubsequentLetters) where
  validFirstLetters = ['a'..'z'] <> ['A'..'Z'] <> ['.', '_', ':', '$']
  validSubsequentLetters = validFirstLetters <> ['0'..'9']

ainstruction :: (MonadState s m, MonadPlus m, WithSource s) => m String
ainstruction = do
  v <- char '@' *> some digit
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

cinstruction :: (MonadState s m, MonadPlus m, WithSource s) => m String
cinstruction = do
  d <- dest <* char '=' <|> return "000"
  c <- comp
  j <- char ';' *> jump <|> return "000"
  return $ "111" <> c <> d <> j 

dest :: (MonadState s m, MonadPlus m, WithSource s) => m String 
dest = foldr f empty
  [ ("001", "M")
  , ("010", "D") 
  , ("011", "MD") 
  , ("100", "A") 
  , ("101", "AM")
  , ("110", "AD")
  , ("111", "AMD")
  ] where f (code, symbol) rest = code <$ string symbol <|> rest

comp :: (MonadState s m, MonadPlus m, WithSource s) => m String 
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

jump :: (MonadState s m, MonadPlus m, WithSource s) => m String 
jump = foldr f empty
  [ ("001", "JGT")
  , ("010", "JEQ") 
  , ("011", "JGE") 
  , ("100", "JLT") 
  , ("101", "JNE") 
  , ("110", "JLE") 
  , ("111", "JMP") 
  ] where f (code, symbol) rest = code <$ string symbol <|> rest

