module Lib (InputFile (..), OutputFile (..), FilePrefix (..), compileFile, liftMaybe, headMay, mapLeft, Source (..)) where

import Control.Applicative (Alternative, empty)
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStrLn, withFile)

newtype InputFile = InputFile String
newtype OutputFile = OutputFile String
newtype FilePrefix = FilePrefix String

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

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

newtype Source = Source {getSource :: String}

-- passState :: (s -> a) -> State s a
-- passState = gets
