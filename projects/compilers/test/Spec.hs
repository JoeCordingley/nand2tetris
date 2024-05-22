import AssemblerSpec
import Test.Tasty (defaultMain, testGroup)
import VMTranslatorSpec (testVMTranslator)

main :: IO ()
main =
    defaultMain $
        testGroup
            "All Tests"
            [ testAssembler
            , testVMTranslator
            ]
