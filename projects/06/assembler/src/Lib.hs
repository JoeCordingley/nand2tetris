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
import Data.Maybe (maybeToList, fromJust)
import Data.Functor (void)
import Data.Functor.Compose (Compose(..))
import Data.Function ((&))
import Control.Monad.State.Lazy (StateT, State, lift, modify, evalStateT, runStateT, MonadState, evalState, state, gets)
import Control.Monad (join, MonadPlus)
import Parser
import Control.Lens (Lens', (%=), use, uses, over, view)
import Data.Map as Map (insert, Map, lookup)

data InputFile = InputFile String
data OutputFile = OutputFile String

type Parser = StateT FirstPassState []
type SymbolTable = Map String String

incrementInstructionCount :: FirstPassState -> FirstPassState
incrementInstructionCount = undefined

firstPassSymbolTableLens :: Lens' FirstPassState SymbolTable
firstPassSymbolTableLens = undefined

secondPassSymbolTableLens :: Lens' SecondPassState SymbolTable
secondPassSymbolTableLens = undefined

nextRamAddressLens :: Lens' SecondPassState Int
nextRamAddressLens = undefined

instance WithSource FirstPassState where
  sourceLens f s = fmap (\so -> s{source = so}) $ f $ source s

data FirstPassState = FirstPassState { instructionCount :: Int, firstPassSymbolTable :: SymbolTable, source :: Maybe String }
data SecondPassState = SecondPassState { nextRamAddress :: Int, secondPassSymbolTable :: SymbolTable}

initialFirstPassState :: String -> FirstPassState
initialFirstPassState = undefined

initialSecondPassState :: SymbolTable -> SecondPassState
initialSecondPassState = undefined

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
assemble = fmap (intercalate "\n" . uncurry evalState . fmap initialSecondPassState) 
  . headMay 
  . (map . fmap) firstPassSymbolTable 
  . runStateT (getCompose  instructions) 
  . initialFirstPassState 
  . filterWhitespace  

firstPass :: FirstPassState -> Maybe (State SecondPassState [String], SymbolTable)
firstPass = headMay . (map . fmap) firstPassSymbolTable . runStateT (getCompose  instructions)

headMay :: [a] -> Maybe a
headMay (a:_) = Just a
headMay [] = Nothing

type Assembler = Compose Parser (State SecondPassState)

instructions :: Assembler [String]
instructions = (>>= maybeToList) <$> many line 

line :: Assembler (Maybe String)
line = fmap join (optional codePortion) <* liftParser (optional comment) <* liftParser endOfLine 

liftParser :: Parser a -> Assembler a
liftParser = Compose . fmap pure

comment :: Parser ()
comment = void $ string "//" *> many (ifChar (/= '\n'))

codePortion :: Assembler (Maybe String)
codePortion = (Just <$> instruction) <|> (Nothing <$ liftParser label)

instruction :: Assembler String
instruction = (ainstruction <|> liftParser cinstruction) <* (liftParser $ modify $ incrementInstructionCount)

label :: Parser ()
label = do
  s <- char '(' *> symbol <* char ')'
  i <- gets instructionCount
  let bin = fromJust $ binaryValue (i+1)
  modify $ over firstPassSymbolTableLens (insert s bin) 

symbol :: Parser String
symbol = (:) <$> charIn validFirstLetters <*> many (charIn validSubsequentLetters) where
  validFirstLetters = ['a'..'z'] <> ['A'..'Z'] <> ['.', '_', ':', '$']
  validSubsequentLetters = validFirstLetters <> ['0'..'9']

ainstruction :: Assembler String
ainstruction = liftParser (char '@') *> (liftParser constant <|> symbolValue)

constant :: Parser String
constant = do
  v <- some digit
  case readMaybe v >>= binaryValue of
    Nothing -> empty
    Just a -> return $ "0" <> a 

symbolValue :: Assembler String
symbolValue = Compose $ do
  s <- symbol
  return $ do
    maybeValue <- gets $ (Map.lookup s) . secondPassSymbolTable
    case maybeValue of
      Just value -> pure value
      Nothing -> do
        currentAddress <- gets nextRamAddress 
        let bin = fromJust $ binaryValue currentAddress
        modify $ over secondPassSymbolTableLens (Map.insert s bin) 
        modify $ over nextRamAddressLens (+1)
        return bin

binaryValue :: Int -> Maybe String
binaryValue = pad .  toBin where
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

