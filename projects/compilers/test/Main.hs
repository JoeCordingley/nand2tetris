module Main (main, myParseTest) where

import AssemblerSpec
import Control.Monad.Reader (runReader)
import Control.Monad.State.Lazy (evalStateT)
import Lib (FilePrefix (..))
import Test.Tasty (defaultMain, testGroup)
import Text.Megaparsec (errorBundlePretty, runParserT)
import VMTranslator (Parser)
import VMTranslatorSpec (testVMTranslator)

main :: IO ()
main =
    defaultMain $
        testGroup
            "All Tests"
            [ testAssembler
            , testVMTranslator
            ]

myParseTest :: Parser String -> String -> IO ()
myParseTest p s = case flip runReader (FilePrefix "null") . flip evalStateT 0 $ runParserT p "sourceName" s of
    Left error -> putStr $ errorBundlePretty error
    Right s -> putStrLn s
