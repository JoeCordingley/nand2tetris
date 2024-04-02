import AssemblerSpec
import Test.Tasty
import VMTranslatorSpec (testVMTranslator)

main :: IO ()
main =
    defaultMain $
        testGroup
            "All Tests"
            [ testAssembler
            , testVMTranslator
            ]
