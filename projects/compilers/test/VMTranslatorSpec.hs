module VMTranslatorSpec (testVMTranslator) where

import Data.List (intercalate)
import Test.Tasty
import Test.Tasty.HUnit
import VMTranslator (translate)

testVMTranslator :: TestTree
testVMTranslator = testGroup "VMTranslator tests" [simpleAdd]

simpleAdd :: TestTree
simpleAdd = testCase "SimpleAdd.vm" $ translate input @?= Just output
  where
    input =
        intercalate
            "\n"
            [ "// This file is part of www.nand2tetris.org"
            , "// and the book \"The Elements of Computing Systems\""
            , "// by Nisan and Schocken, MIT Press."
            , "// File name: projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm"
            , ""
            , "// Pushes and adds two constants."
            , "push constant 7"
            , "push constant 8"
            , "add"
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
            , "M=M-1"
            , "A=M"
            , "D=M"
            , "@SP"
            , "M=M-1"
            , "A=M"
            , "M=D+M"
            , "@SP"
            , "M=M+1"
            ]
