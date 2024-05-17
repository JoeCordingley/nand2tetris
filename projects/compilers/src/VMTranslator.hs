{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module VMTranslator (translateFile, translate, incrementing) where

import Control.Monad.Reader (ReaderT (..), ask, mapReaderT)
import Control.Monad.State.Lazy (StateT, evalStateT, get, lift, put)
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Void
import Lib (FilePrefix (..), InputFile (..), OutputFile (..), liftMaybe, mapLeft)
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStrLn, withFile)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (digitChar, eol, hspace1, letterChar, string)
import Text.Megaparsec.Char.Lexer
import Text.Read (readMaybe)

type Parser = ParsecT Void String (Incrementing (WithFilePrefix Identity))

type Incrementing m = StateT Int m
type WithFilePrefix m = ReaderT FilePrefix m

translateFile :: FilePrefix -> InputFile -> OutputFile -> IO ()
translateFile filePrefix = compileFile $ flip runReaderT filePrefix . translate

translate :: String -> ReaderT FilePrefix (Either String) String
translate = fmap (intercalate "\n") . runTrans instructions

runTrans :: Parser a -> String -> ReaderT FilePrefix (Either String) a
runTrans p = mapReaderT runIdentity . fmap (mapLeft errorBundlePretty) . flip evalStateT 0 . runParserT p "sourceName"

line :: Parser [String]
line = fmap concat . lexeme' . optional $ vminstruction

lines' :: (MonadParsec e String f) => f [a] -> f [a]
lines' l = (<>) <$> l <*> rest
  where
    rest = [] <$ eof <|> eol *> lines' l

instructions :: Parser [String]
instructions = lines' $ whitespace *> line

whitespace :: Parser ()
whitespace = space hspace1 comment empty

lexeme' :: Parser a -> Parser a
lexeme' = lexeme whitespace

compileFile :: (String -> Either String String) -> InputFile -> OutputFile -> IO ()
compileFile compile (InputFile input) (OutputFile output) =
    withFile input ReadMode $ \i ->
        withFile output WriteMode $ \o -> do
            contents <- hGetContents i
            case compile contents of
                Right compiled -> hPutStrLn o compiled *> putStrLn "success"
                Left errorBundle -> putStr errorBundle

comment :: Parser ()
comment = skipLineComment "//"

data Direction = Push | Pop

int :: Parser Int
int = do
    digits <- many digitChar
    liftMaybe $ readMaybe digits

label' :: Parser String
label' = (:) <$> nonDigitChar <*> many labelChar
  where
    nonDigitChar = letterChar <|> oneOf ['_', '.', ':']
    labelChar = digitChar <|> nonDigitChar

vminstruction :: Parser [String]
vminstruction = memoryCommand <|> logicalCommand <|> programFlowCommand
  where
    memoryCommand = lexeme' op <*> lexeme' segment <*> lexeme' index
      where
        op = push <|> pop
          where
            push = ($ Push) <$ string "push"
            pop = ($ Pop) <$ string "pop"
        segment = constant <|> local <|> argument <|> this <|> that <|> temp <|> pointer <|> static
          where
            constant = f <$ string "constant"
              where
                f Push i = ["@" <> show i, "D=A"] <> pushd
                f Pop _ = ["@SP", "M=M-1"]
            temp = rSegment 5 <$ string "temp"
            pointer = rSegment 3 <$ string "pointer"
            static = withFilePrefix g
              where
                g (FilePrefix filePrefix) = f <$ string "static"
                  where
                    f Push i = ["@" <> filePrefix <> "." <> show i, "D=M"] <> pushd
                    f Pop i = popd <> ["@" <> filePrefix <> "." <> show i, "M=D"]
            local = segmentCommand "LCL" <$ string "local"
            argument = segmentCommand "ARG" <$ string "argument"
            this = segmentCommand "THIS" <$ string "this"
            that = segmentCommand "THAT" <$ string "that"
            rSegment j Push i = ["@R" <> show (i + j), "D=M"] <> pushd
            rSegment j Pop i = popd <> ["@R" <> show (i + j), "M=D"]
            segmentCommand seg Push i = ["@" <> seg, "D=M", "@" <> show i, "A=D+A", "D=M"] <> pushd
            segmentCommand seg Pop i = ["@" <> seg, "D=M", "@" <> show i, "D=D+A", "@R13", "M=D"] <> popd <> ["@R13", "A=M", "M=D"]
        index = int
    logicalCommand = lexeme' $ add <|> eq <|> lt <|> gt <|> sub <|> neg <|> and' <|> or' <|> not'
      where
        add = simpleBinaryOperator "D=D+M" "add"
        sub = simpleBinaryOperator "D=M-D" "sub"
        eq = relationalOperator "EQ" "eq"
        lt = relationalOperator "LT" "lt"
        gt = relationalOperator "GT" "gt"
        and' = simpleBinaryOperator "D=D&M" "and"
        or' = simpleBinaryOperator "D=D|M" "or"
        neg = unaryOp "D=-M" "neg"
        not' = unaryOp "D=!M" "not"
        simpleBinaryOperator f = binaryOperator [f]
        relationalOperator f name = incrementing g
          where
            g i =
                binaryOperator
                    [ "D=M-D"
                    , "@" <> eqi
                    , "D;J" <> f
                    , "D=0"
                    , "@" <> endi
                    , "0;JMP"
                    , "(" <> eqi <> ")"
                    , "D=-1"
                    , "(" <> endi <> ")"
                    ]
                    name
              where
                eqi = "EQ" <> show i
                endi = "END" <> show i
        unaryOp f name = popm <> [f] <> pushd <$ string name
        binaryOperator commands name = popd <> popm <> commands <> pushd <$ string name
        popm =
            [ "@SP"
            , "AM=M-1"
            ]
    programFlowCommand = labelCommand <|> ifgoto <|> goto
      where
        labelCommand = fmap f $ lexeme' (string "label") *> lexeme' label'
          where
            f label'' = ["(" <> label'' <> ")"]
        ifgoto = fmap f $ lexeme' (string "if-goto") *> lexeme' label'
          where
            f label'' = popd <> ["@" <> label'', "D;JNE"]
        goto = fmap f $ lexeme' (string "goto") *> lexeme' label'
          where
            f label'' = ["@" <> label'', "0;JMP"]
    pushd = ["@SP", "A=M", "M=D", "@SP", "M=M+1"]
    popd =
        [ "@SP"
        , "AM=M-1"
        , "D=M"
        ]

incrementing :: (Int -> Parser a) -> Parser a
incrementing f = do
    i <- lift get
    f i <* (lift . put $ i + 1)

withFilePrefix :: (FilePrefix -> Parser a) -> Parser a
withFilePrefix f = lift ask >>= f
