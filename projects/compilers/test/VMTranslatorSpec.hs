module VMTranslatorSpec (testVMTranslator) where

import Lib (FilePrefix (..), Source (..))
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, hGetLine, hIsEOF, openFile)
import Test.Tasty
import Test.Tasty.HUnit
import VMTranslator (functionsInstructions, innerFunctionInstructions)

testVMTranslator :: TestTree
testVMTranslator =
    testGroup "VMTranslator tests" $
        fmap
            f
            [ ("../07/StackArithmetic/SimpleAdd/", "SimpleAdd", innerFunctions)
            , ("../07/StackArithmetic/StackTest/", "StackTest", innerFunctions)
            , ("../07/MemoryAccess/BasicTest/", "BasicTest", innerFunctions)
            , ("../07/MemoryAccess/PointerTest/", "PointerTest", innerFunctions)
            , ("../07/MemoryAccess/StaticTest/", "StaticTest", innerFunctions)
            , ("../08/ProgramFlow/BasicLoop/", "BasicLoop", innerFunctions)
            , ("../08/ProgramFlow/FibonacciSeries/", "FibonacciSeries", innerFunctions)
            , ("../08/FunctionCalls/SimpleFunction/", "SimpleFunction", functionsInstructions)
            ]
  where
    f (path, prefix, parser) = fileCompare path prefix parser
    innerFunctions = innerFunctionInstructions "null"

type ParseFunction = FilePrefix -> Source -> Either String [String]

fileCompare :: FilePath -> String -> ParseFunction -> TestTree
fileCompare filePath filePrefix parseFunction = withResource acquireFiles closeFiles testParser'
  where
    acquireFiles = (,) <$> acquireFile (filePath <> filePrefix <> ".vm") (fmap Source . hGetContents) <*> acquireFile (filePath <> filePrefix <> ".asm") hGetLines
    acquireFile path f = do
        handle <- openFile path ReadMode
        contents <- f handle
        return (handle, contents)
    closeFiles (x, y) = closeFile x *> closeFile y
    closeFile = hClose . fst
    testParser' = testParser . fmap f
      where
        f ((_, source), (_, lines')) = (source, lines')
        testParser io = testCase filePrefix $ do
            (source, output) <- io
            parseFunction (FilePrefix filePrefix) source @?= Right output

hGetLines :: Handle -> IO [String]
hGetLines handle = whileM (not <$> hIsEOF handle) (hGetLine handle)

whileM :: (Monad m) => m Bool -> m a -> m [a]
whileM p ma = do
    b <- p
    if b then (:) <$> ma <*> whileM p ma else return []
