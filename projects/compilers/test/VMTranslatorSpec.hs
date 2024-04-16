module VMTranslatorSpec (testVMTranslator) where

import Data.List (intercalate)
import Test.Tasty
import Test.Tasty.HUnit
import VMTranslator (translate)

testVMTranslator :: TestTree
testVMTranslator = testGroup "VMTranslator tests" [simpleAdd]

simpleAdd :: TestTree
simpleAdd = testCase "SimpleAdd.vm" $ translate input @?= Right output
  where
    input =
        "push constant 7 \n"
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
