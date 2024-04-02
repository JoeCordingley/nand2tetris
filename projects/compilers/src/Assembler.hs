module Assembler (
    assembleFile,
    assemble,
) where

import Control.Applicative (empty, many, some, (<|>))
import Control.Monad (join)
import Control.Monad.State.Lazy (State, evalState, evalStateT, gets, modify, runState, state)
import Data.Char (intToDigit)
import Data.Functor (void)
import Data.Functor.Compose (Compose (..))
import Data.List (intercalate)
import Data.Map as Map (Map, fromList, insert, lookup)
import Data.Maybe (catMaybes, fromJust)
import Lib
import Numeric (showIntAtBase)
import Parser
import Text.Read (readMaybe)

type SymbolTable = Map String String

data FirstPassState = FirstPassState {instructionCount :: Int, firstPassSymbolTable :: SymbolTable}
data SecondPassState = SecondPassState {nextRamAddress :: Int, secondPassSymbolTable :: SymbolTable}

initialFirstPassState :: FirstPassState
initialFirstPassState = FirstPassState{instructionCount = 0, firstPassSymbolTable = initialSymbolTable}

initialSymbolTable :: Map String String
initialSymbolTable =
    fmap (fromJust . binaryValue) . Map.fromList $
        [ ("SP", 0)
        , ("LCL", 1)
        , ("ARG", 2)
        , ("THIS", 3)
        , ("THAT", 4)
        ]
            <> map rvalue [0 .. 15]
            <> [("SCREEN", 16384), ("KBD", 24576)]
  where
    rvalue n = ("R" <> show n, n)

initialSecondPassState :: SymbolTable -> SecondPassState
initialSecondPassState symbolTable = SecondPassState{nextRamAddress = 16, secondPassSymbolTable = symbolTable}

assembleFile :: InputFile -> OutputFile -> IO ()
assembleFile = compileFile assemble

filterWhitespace :: String -> String
filterWhitespace = filter (/= ' ')

assemble :: String -> Maybe String
assemble = fmap (intercalate "\n" . runSecondPass . runFirstPass) . parse . filterWhitespace
  where
    parse = headMay . evalStateT (getCompose instructions) . Just
    runFirstPass = flip runState initialFirstPassState . getCompose
    runSecondPass = uncurry evalState . fmap (initialSecondPassState . firstPassSymbolTable)

headMay :: [a] -> Maybe a
headMay (a : _) = Just a
headMay [] = Nothing

type FirstPass = State FirstPassState
type SecondPass = State SecondPassState
type Assembler = Compose Parser (Compose FirstPass SecondPass)

instructions :: Assembler [String]
instructions = catMaybes <$> many line

line :: Assembler (Maybe String)
line = fmap join (optional codePortion) <* liftOuter (optional comment) <* liftOuter endOfLine

comment :: Parser ()
comment = void $ string "//" *> many (ifChar (/= '\n'))

codePortion :: Assembler (Maybe String)
codePortion = Just <$> instruction <|> Nothing <$ label

instruction :: Assembler String
instruction =
    (ainstruction <|> liftOuter cinstruction)
        <* (liftInner . liftOuter . modify $ incrementInstructionCount)
  where
    incrementInstructionCount s = s{instructionCount = instructionCount s + 1}

label :: Assembler ()
label = Compose $ do
    s <- char '(' *> symbol <* char ')'
    pure . liftOuter $ do
        i <- gets (fromJust . binaryValue . instructionCount)
        modify $ overSymbolTable (insert s i)
  where
    overSymbolTable f s = s{firstPassSymbolTable = f (firstPassSymbolTable s)}

ainstruction :: Assembler String
ainstruction = liftOuter (char '@') *> (liftOuter constant <|> symbolValue)

constant :: Parser String
constant = do
    v <- some digit
    maybe empty return (readMaybe v >>= binaryValue)

symbolValue :: Assembler String
symbolValue = Compose $ do
    s <- symbol
    return $ liftInner $ do
        maybeValue <- gets $ Map.lookup s . secondPassSymbolTable
        case maybeValue of
            Just value -> pure value
            Nothing -> state (nextAddress s)
  where
    nextAddress s (SecondPassState currentAddress symbols) =
        ( binaryAddress
        , SecondPassState
            { nextRamAddress = currentAddress + 1
            , secondPassSymbolTable = Map.insert s binaryAddress symbols
            }
        )
      where
        binaryAddress = fromJust $ binaryValue currentAddress

liftOuter :: (Applicative g, Functor f) => f a -> Compose f g a
liftOuter = Compose . fmap pure

liftInner :: (Applicative f) => g a -> Compose f g a
liftInner = Compose . pure

symbol :: Parser String
symbol = (:) <$> charIn validFirstLetters <*> many (charIn validSubsequentLetters)
  where
    validFirstLetters = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['.', '_', ':', '$']
    validSubsequentLetters = validFirstLetters <> ['0' .. '9']

binaryValue :: Int -> Maybe String
binaryValue = pad . toBin
  where
    pad b = case 15 - length b of
        n
            | n >= 0 -> Just $ replicate (n + 1) '0' <> b
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
    foldr
        (uncurry foldParse)
        empty
        [ ("001", "M")
        , ("010", "D")
        , ("011", "MD")
        , ("100", "A")
        , ("101", "AM")
        , ("110", "AD")
        , ("111", "AMD")
        ]

comp :: Parser String
comp = dComp <|> mComp
  where
    dComp = ('0' :) <$> comp' "A"
    mComp = ('1' :) <$> comp' "M"
    comp' aOrM =
        foldr
            (uncurry foldParse)
            empty
            [ ("101010", "0")
            , ("111111", "1")
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
            ]

jump :: Parser String
jump =
    foldr
        (uncurry foldParse)
        empty
        [ ("001", "JGT")
        , ("010", "JEQ")
        , ("011", "JGE")
        , ("100", "JLT")
        , ("101", "JNE")
        , ("110", "JLE")
        , ("111", "JMP")
        ]

foldParse :: a -> String -> Parser a -> Parser a
foldParse a s p = a <$ string s <|> p
