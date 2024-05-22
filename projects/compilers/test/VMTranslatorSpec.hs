module VMTranslatorSpec (testVMTranslator) where

import Control.Monad.Reader (runReaderT)
import Data.List (intercalate)
import Lib (FilePrefix (..), Source (..))
import Test.Tasty
import Test.Tasty.HUnit
import VMTranslator (translateInnerFunction)

testVMTranslator :: TestTree
testVMTranslator = testGroup "VMTranslator tests" [simpleAdd]

simpleAdd :: TestTree
simpleAdd = testCase "SimpleAdd.vm" $ translateInnerFunction "null" (FilePrefix "file") input @?= Right output
  where
    input =
        Source $
            intercalate
                "\n"
                [ "// This file is part of www.nand2tetris.org"
                , "// and the book \"The Elements of Computing Systems\""
                , "// by Nisan and Schocken, MIT Press."
                , "// File name: projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm"
                , ""
                , "// Pushes and adds two constants."
                , "push constant 7      //comment"
                , "push constant 8 //comment"
                , "add //comment"
                , "//comment"
                ]
    output =
        intercalate
            "\n"
            [ "@7"
            , "D=A"
            , "@SP"
            , "A=M"
            , "M=D"
            , "@SP"
            , "M=M+1"
            , "@8"
            , "D=A"
            , "@SP"
            , "A=M"
            , "M=D"
            , "@SP"
            , "M=M+1"
            , "@SP"
            , "AM=M-1"
            , "D=M"
            , "@SP"
            , "AM=M-1"
            , "D=D+M"
            , "@SP"
            , "A=M"
            , "M=D"
            , "@SP"
            , "M=M+1"
            ]
