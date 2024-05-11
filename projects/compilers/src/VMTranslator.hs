{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module VMTranslator (translateFile, translate) where

import Control.Monad.State.Lazy (State, evalState, get, lift, put)
import Data.List (intercalate)
import Data.Void
import Lib (InputFile (..), OutputFile (..), liftMaybe, mapLeft)
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStrLn, withFile)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (digitChar, eol, hspace1, string)
import Text.Megaparsec.Char.Lexer
import Text.Read (readMaybe)

type Parser = ParsecT Void String (State Int)

translateFile :: String -> InputFile -> OutputFile -> IO ()
translateFile filePrefix = compileFile $ translate filePrefix

translate :: String -> String -> Either String String
translate filePrefix = fmap (intercalate "\n") . runTrans (instructions filePrefix)

runTrans :: Parser a -> String -> Either String a
runTrans p = mapLeft errorBundlePretty . flip evalState 0 . runParserT p "sourceName"

line :: String -> Parser [String]
line filePrefix = fmap concat . word . optional $ vminstruction filePrefix

lines' :: (MonadParsec e String f) => f [a] -> f [a]
lines' l = (<>) <$> l <*> rest
  where
    rest = [] <$ eof <|> eol *> lines' l

instructions :: String -> Parser [String]
instructions filePrefix = lines' $ line filePrefix

word :: Parser a -> Parser a
word = lexeme (space hspace1 comment empty)

compileFile :: (String -> Either String String) -> InputFile -> OutputFile -> IO ()
compileFile compile (InputFile input) (OutputFile output) =
    withFile input ReadMode $ \i ->
        withFile output WriteMode $ \o -> do
            contents <- hGetContents i
            case compile contents of
                Right compiled -> hPutStrLn o compiled *> putStrLn "success"
                Left errorBundle -> print $ show errorBundle

comment :: Parser ()
comment = skipLineComment "//"

data Direction = Push | Pop

int :: Parser Int
int = do
    digits <- many digitChar
    liftMaybe $ readMaybe digits

vminstruction :: String -> Parser [String]
vminstruction filePrefix = memoryCommand <|> logicalCommand
  where
    memoryCommand = word op <*> word segment <*> word index
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
            static = f <$ string "static"
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
    logicalCommand = word $ add <|> eq <|> lt <|> gt <|> sub <|> neg <|> and' <|> or' <|> not'
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
