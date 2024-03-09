import Test.HUnit

-- Define a function that you want to test
-- For example, let's say we have a simple function to add two numbers
add :: Int -> Int -> Int
add x y = x + y

-- Write test cases using HUnit assertions
testAddition :: Test
testAddition = "Addition Test" ~: TestList
    [ "1 + 1 = 2" ~: add 1 1 ~?= 2
    , "2 + 3 = 5" ~: add 2 3 ~?= 5
    , "(-1) + 1 = 0" ~: add (-1) 1 ~?= 0
    ]

-- Run the tests
main :: IO ()
main = do
    -- Run the test cases and print the results
    putStrLn "Running tests..."
    _ <- runTestTT (TestList [testAddition])
    putStrLn "Tests completed."

