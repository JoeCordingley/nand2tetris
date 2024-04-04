module Lib (InputFile (..), OutputFile (..), compileFile, liftMaybe, headMay) where

import Control.Applicative (Alternative, empty)
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStrLn, withFile)

newtype InputFile = InputFile String
newtype OutputFile = OutputFile String

compileFile :: (String -> Maybe String) -> InputFile -> OutputFile -> IO ()
compileFile compile (InputFile input) (OutputFile output) =
    withFile input ReadMode $ \i ->
        withFile output WriteMode $ \o -> do
            contents <- hGetContents i
            case compile contents of
                Just compiled -> hPutStrLn o compiled *> putStrLn "success"
                Nothing -> putStrLn "failure"

liftMaybe :: (Alternative f) => Maybe a -> f a
liftMaybe (Just a) = pure a
liftMaybe Nothing = empty

headMay :: [a] -> Maybe a
headMay (a : _) = Just a
headMay [] = Nothing

-- passState :: (s -> a) -> State s a
-- passState = gets
