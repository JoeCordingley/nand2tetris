{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module VMTranslator (translateInnerFunctionFile, translateFunctionFile, innerFunctionInstructions, incrementing, vminstruction, Parser, line) where

import Control.Monad.Reader (ReaderT (..), ask, mapReaderT)
import Control.Monad.State.Lazy (StateT, evalStateT, get, lift, put)
import Data.Functor (void)
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Void
import Lib (FilePrefix (..), InputFile (..), OutputFile (..), Source (..), liftMaybe, mapLeft)
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStrLn, withFile)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (digitChar, eol, hspace1, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer
import Text.Read (readMaybe)

type Parser = ParsecT Void String (Incrementing (WithFilePrefix Identity))

type Incrementing m = StateT Int m
type WithFilePrefix m = ReaderT FilePrefix m

translateFile :: (a -> Source -> Either String [String]) -> a -> InputFile -> OutputFile -> IO ()
translateFile assemblyInstructions = compileFile . translateString
  where
    translateString a source = intercalate "\n" <$> assemblyInstructions a source

translateInnerFunctionFile :: FunctionName -> FilePrefix -> InputFile -> OutputFile -> IO ()
translateInnerFunctionFile functionName = translateFile (innerFunctionInstructions functionName)

translateFunctionFile :: FilePrefix -> InputFile -> OutputFile -> IO ()
translateFunctionFile = translateFile functionsInstructions

innerFunctionInstructions :: FunctionName -> FilePrefix -> Source -> Either String [String]
innerFunctionInstructions functionName filePrefix = flip runReaderT filePrefix . runTrans (instructions functionName)

functionsInstructions :: FilePrefix -> Source -> Either String [String]
functionsInstructions filePrefix = flip runReaderT filePrefix . runTrans functions

runTrans :: Parser a -> Source -> ReaderT FilePrefix (Either String) a
runTrans p = mapReaderT runIdentity . fmap (mapLeft errorBundlePretty) . flip evalStateT 0 . runParserT p "sourceName" . getSource

untilEof :: (MonadParsec e String f) => f a -> f [a]
untilEof p = [] <$ eof <|> (:) <$> p <*> untilEof p

line :: Parser a -> Parser a
line p = hwhitespace *> lexeme' p <* (void eol <|> eof)

instructions :: FunctionName -> Parser [String]
instructions functionName = fmap concat . untilEof . line . fmap concat . optional $ vminstruction functionName

functions :: Parser [String]
functions = fmap concat $ whitespace *> untilEof (lexeme whitespace function)

-- functions = lexeme whitespace function

whitespace :: Parser ()
whitespace = space space1 comment empty

withTrailingBlankLines :: Parser a -> Parser a
withTrailingBlankLines p = p <* many (line hwhitespace)

function :: Parser [String]
function = do
    (functionName, i) <- line $ (,) <$> (lexeme' (string "function") *> lexeme' label') <*> int
    instructions' <- fmap concat . many . try . line . fmap concat . optional $ vminstruction functionName
    void . line $ string "return"
    return $ functionInit functionName i <> instructions' <> functionReturn
  where
    functionInit functionName i = "(" <> functionName <> ")" : concat (replicate i $ pushConstant 0)
    functionReturn =
        ["@LCL", "D=M", "@FRAME", "M=D"] -- FRAME = LCL
            <> ["@5", "A=D-A", "D=M", "@RET", "M=D"] -- RET = *(FRAME-5)
            <> popd
            <> ["@ARG", "A=M", "M=D"] -- ARG = pop()
            <> ["D=A+1", "@SP", "M=D"] -- SP = ARG+1
            <> ["@FRAME", "A=M-1", "D=M", "@THAT", "M=D"] -- THAT = *(FRAME-1)
            <> ["@FRAME", "D=M", "@2", "A=D-A", "D=M", "@THIS", "M=D"] -- THIS = *(FRAME-2)
            <> ["@FRAME", "D=M", "@3", "A=D-A", "D=M", "@ARG", "M=D"] -- ARG = *(FRAME-3)
            <> ["@FRAME", "D=M", "@4", "A=D-A", "D=M", "@LCL", "M=D"] -- LCL = *(FRAME-4)
            <> ["@RET", "A=M", "0;JMP"] -- goto RET

hwhitespace :: Parser ()
hwhitespace = space hspace1 comment empty

lexeme' :: Parser a -> Parser a
lexeme' = lexeme hwhitespace

compileFile :: (Source -> Either String String) -> InputFile -> OutputFile -> IO ()
compileFile compile (InputFile input) (OutputFile output) =
    withFile input ReadMode $ \i ->
        withFile output WriteMode $ \o -> do
            contents <- hGetContents i
            case compile $ Source contents of
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

type FunctionName = String

pushConstant :: Int -> [String]
pushConstant i = ["@" <> show i, "D=A"] <> pushd

pushd :: [String]
pushd = ["@SP", "A=M", "M=D", "@SP", "M=M+1"]

popd :: [String]
popd =
    [ "@SP"
    , "AM=M-1"
    , "D=M"
    ]

vminstruction :: FunctionName -> Parser [String]
vminstruction functionName = memoryCommand <|> logicalCommand <|> programFlowCommand
  where
    memoryCommand = lexeme' op <*> lexeme' segment <*> index
      where
        op = push <|> pop
          where
            push = ($ Push) <$ string "push"
            pop = ($ Pop) <$ string "pop"
        segment = constant <|> local <|> argument <|> this <|> that <|> temp <|> pointer <|> static
          where
            constant = f <$ string "constant"
              where
                f Push i = pushConstant i
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
    programFlowCommand = labelCommand <|> ifgoto <|> goto
      where
        labelCommand = fmap f $ lexeme' (string "label") *> label'
          where
            f label'' = ["(" <> labelString label'' <> ")"]
        ifgoto = fmap f $ lexeme' (string "if-goto") *> label'
          where
            f label'' = popd <> ['@' : labelString label'', "D;JNE"]
        goto = fmap f $ lexeme' (string "goto") *> label'
          where
            f label'' = ['@' : labelString label'', "0;JMP"]
        labelString label'' = functionName <> "." <> label''

incrementing :: (Int -> Parser a) -> Parser a
incrementing f = do
    i <- lift get
    f i <* (lift . put $ i + 1)

withFilePrefix :: (FilePrefix -> Parser a) -> Parser a
withFilePrefix f = lift ask >>= f
