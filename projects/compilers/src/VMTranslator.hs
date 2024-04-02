module VMTranslator (translateFile, translate) where

import Control.Applicative (many, (<|>))
import Data.Functor (void)
import Data.List (intercalate)
import Lib
import Parser

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
        relationalOperator f =
            binaryOperator
                [ "D=D-M"
                , "@EQ"
                , "D;J" <> f
                , "D=0"
                , "@END"
                , "0;JMP"
                , "(EQ)"
                , "D=-1"
                , "(END)"
                ]
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
