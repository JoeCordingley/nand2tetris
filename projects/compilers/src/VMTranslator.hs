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
line = fmap concat (optional vminstruction) <* optional comment <* endOfLine

comment :: Parser ()
comment = void $ string "//" *> many (ifChar (/= '\n'))

vminstruction :: Parser [String]
vminstruction = push <|> command
  where
    push = token (string "push") *> token (constant <$ string "constant") <*> index
      where
        index = int
        constant i = ["@" <> show i, "D=A"] <> pushd
    command = add <|> eq <|> lt <|> gt <|> sub <|> neg <|> and' <|> or' <|> not'
      where
        binaryOperator f name = popd <> popm <> f <> pushd <$ string name
        relationalOperator :: String -> String -> Parser [String]
        relationalOperator f name =
            incrementing $ flip binaryOperator name . relationalCommands f
        relationalCommands :: String -> Int -> [String]
        relationalCommands f i =
            [ "D=D-M"
            , "@" <> eqi
            , "D;J" <> f
            , "D=0"
            , "@" <> endi
            , "0;JMP"
            , "(" <> eqi <> ")"
            , "D=-1"
            , "(" <> endi <> ")"
            ]
          where
            eqi = "EQ" <> show i
            endi = "END" <> show i

        add = binaryOperator ["D=D+M"] "add"
        sub = binaryOperator ["D=D-M"] "sub"
        eq = relationalOperator "EQ" "eq"
        lt = relationalOperator "LT" "lt"
        gt = relationalOperator "GT" "gt"
        and' = binaryOperator ["D=D&M"] "and"
        or' = binaryOperator ["D=D|M"] "or"
        popd =
            [ "@SP"
            , "AM=M-1"
            , "D=M"
            ]
        popm =
            [ "@SP"
            , "AM=M-1"
            ]
        neg = popm <> ["D=-M"] <> pushd <$ string "neg"
        not' = popm <> ["D=!M"] <> pushd <$ string "not"
    pushd = ["@SP", "A=M", "M=D", "@SP", "M=M+1"]

incrementing :: (Int -> Parser a) -> Parser a
incrementing f = do
    i <- lift get
    f i <* lift (put i)
