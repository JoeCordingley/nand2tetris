{-# LANGUAGE FlexibleContexts #-}

module VMTranslator (translateFile, translate) where

import Control.Applicative (many, (<|>))
import Control.Monad.State (StateT, get, lift, put)
import Control.Monad.State.Lazy (evalStateT)
import Data.Functor (void)
import Data.List (intercalate)
import Lib
import Parser hiding (Parser, runParser)

runParser :: Parser a -> String -> Maybe a
runParser p = headMay . flip evalStateT 0 . evalStateT p . Just

type Parser = StateT (Maybe String) (StateT Int [])

translateFile :: InputFile -> OutputFile -> IO ()
translateFile = compileFile translate

translate :: String -> Maybe String
translate = fmap (intercalate "\n") . runParser (concat <$> many line)

line :: Parser [String]
line = fmap concat (optional . lexeme $ vminstruction) <* optional comment <* endOfLine

comment :: Parser ()
comment = void $ string "//" *> many (ifChar (/= '\n'))

data Direction = Push | Pop

vminstruction :: Parser [String]
vminstruction = memoryCommand <|> logicalCommand
  where
    memoryCommand = lexeme op <*> lexeme segment <*> index
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
                f Push i = ["@Xxx." <> show i, "D=M"] <> pushd
                f Pop i = popd <> ["@Xxx." <> show i, "M=D"]
            local = segmentCommand "LCL" <$ string "local"
            argument = segmentCommand "ARG" <$ string "argument"
            this = segmentCommand "THIS" <$ string "this"
            that = segmentCommand "THAT" <$ string "that"
            rSegment j Push i = ["@R" <> show (i + j), "D=M"] <> pushd
            rSegment j Pop i = popd <> ["@R" <> show (i + j), "M=D"]
            segmentCommand seg Push i = ["@" <> seg, "D=M", "@" <> show i, "A=D+A", "D=M"] <> pushd
            segmentCommand seg Pop i = ["@" <> seg, "D=M", "@" <> show i, "D=D+A", "@R13", "M=D"] <> popd <> ["@R13", "A=M", "M=D"]
        index = int
    logicalCommand = add <|> eq <|> lt <|> gt <|> sub <|> neg <|> and' <|> or' <|> not'
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
