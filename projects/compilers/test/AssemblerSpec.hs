module AssemblerSpec (testAssembler) where

import Assembler
import Test.Tasty
import Test.Tasty.HUnit

instructionsAndOutputs :: [(String, String)]
instructionsAndOutputs =
    cinstructionsAndOutputs
        <> ainstructionsAndOutputs
        <> commentsAndBlankLines
        <> commentsAndCode

commentsAndBlankLines :: [(String, String)]
commentsAndBlankLines =
    [ (" ", "")
    , ("  // this is a comment", "")
    ]

ainstructionsAndOutputs :: [(String, String)]
ainstructionsAndOutputs =
    [ ("@0", "0000000000000000")
    , ("@32767", "0111111111111111")
    ]

commentsAndCode :: [(String, String)]
commentsAndCode =
    [("// comment\r\n A=D //comment\r\n", "1110001100100000")]

cinstructionsAndOutputs :: [(String, String)]
cinstructionsAndOutputs =
    [ (dIn <> cIn <> jIn, "111" <> cOut <> dOut <> jOut)
    | (dIn, dOut) <- dests
    , (cIn, cOut) <- comps
    , (jIn, jOut) <- jumps
    ]
  where
    dests =
        [ ("", "000")
        , ("M=", "001")
        , ("D=", "010")
        , ("MD=", "011")
        , ("A=", "100")
        , ("AM=", "101")
        , ("AD=", "110")
        , ("AMD=", "111")
        ]
    comps =
        [ ("0", "0101010")
        , ("1", "0111111")
        , ("-1", "0111010")
        , ("D", "0001100")
        , ("A", "0110000")
        , ("!D", "0001101")
        , ("!A", "0110001")
        , ("-D", "0001111")
        , ("-A", "0110011")
        , ("D+1", "0011111")
        , ("A+1", "0110111")
        , ("D-1", "0001110")
        , ("A-1", "0110010")
        , ("D+A", "0000010")
        , ("D-A", "0010011")
        , ("A-D", "0000111")
        , ("D&A", "0000000")
        , ("D|A", "0010101")
        , ("M", "1110000")
        , ("!M", "1110001")
        , ("-M", "1110011")
        , ("M+1", "1110111")
        , ("M-1", "1110010")
        , ("D+M", "1000010")
        , ("D-M", "1010011")
        , ("M-D", "1000111")
        , ("D&M", "1000000")
        , ("D|M", "1010101")
        ]
    jumps =
        [ ("", "000")
        , (";JGT", "001")
        , (";JEQ", "010")
        , (";JGE", "011")
        , (";JLT", "100")
        , (";JNE", "101")
        , (";JLE", "110")
        , (";JMP", "111")
        ]

testAssembler :: TestTree
testAssembler =
    testGroup
        "Assembler tests"
        [ testCase command $ assemble command @?= Just output
        | (command, output) <- instructionsAndOutputs
        ]
