module VMTranslatorSpec (testVMTranslator) where

import Lib (FilePrefix (..), Source (..))
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, hGetLine, hIsEOF, openFile)
import Test.Tasty
import Test.Tasty.HUnit
import VMTranslator (innerFunctionInstructions)

testVMTranslator :: TestTree
testVMTranslator =
    testGroup "VMTranslator tests" $
        fmap
            (uncurry f)
            [ ("../07/StackArithmetic/SimpleAdd/", "SimpleAdd")
            , ("../07/StackArithmetic/StackTest/", "StackTest")
            , ("../07/MemoryAccess/BasicTest/", "BasicTest")
            , ("../07/MemoryAccess/PointerTest/", "PointerTest")
            , ("../07/MemoryAccess/StaticTest/", "StaticTest")
            , ("../08/ProgramFlow/BasicLoop/", "BasicLoop")
            , ("../08/ProgramFlow/FibonacciSeries/", "FibonacciSeries")
            ]
  where
    f path prefix = fileCompare path (FilePrefix prefix)

type OutputLines = [String]

testParser :: FilePrefix -> IO (Source, OutputLines) -> TestTree
testParser filePrefix io = testCase (getPrefix filePrefix) $ do
    (source, output) <- io
    innerFunctionInstructions "null" filePrefix source @?= Right output

fileCompare :: FilePath -> FilePrefix -> TestTree
fileCompare filePath filePrefix = withResource acquireFiles closeFiles testParser'
  where
    acquireFiles = (,) <$> acquireFile (filePath <> filePrefix' <> ".vm") (fmap Source . hGetContents) <*> acquireFile (filePath <> filePrefix' <> ".asm") hGetLines
      where
        filePrefix' = getPrefix filePrefix
    acquireFile path f = do
        handle <- openFile path ReadMode
        contents <- f handle
        return (handle, contents)
    closeFiles (x, y) = closeFile x *> closeFile y
    closeFile = hClose . fst
    testParser' = testParser filePrefix . fmap f
      where
        f ((_, source), (_, lines')) = (source, lines')

getPrefix (FilePrefix f) = f

hGetLines :: Handle -> IO [String]
hGetLines handle = whileM (not <$> hIsEOF handle) (hGetLine handle)

whileM :: (Monad m) => m Bool -> m a -> m [a]
whileM p ma = do
    b <- p
    if b then (:) <$> ma <*> whileM p ma else return []
