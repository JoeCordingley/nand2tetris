{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( printFile
    , InputFile(..)
    , OutputFile(..)
    , assemble
    ) where

import System.IO (withFile, IOMode(..))
import Pipes
import Pipes.Prelude (mapFoldable, fromHandle, toHandle)
import Text.Parsec hiding (string)
import qualified Text.Parsec as Parsec (string)
import Control.Monad.Except

data InputFile = InputFile String
data OutputFile = OutputFile String

printFile :: InputFile -> OutputFile -> IO ()
printFile (InputFile input) (OutputFile output) = 
  withFile input ReadMode $ \i -> 
    withFile output WriteMode $ \o -> 
      runEffect $ fromHandle i >-> mapFoldable (ExceptT . assemble)  >-> toHandle o

assemble :: String -> Maybe (Either ParseError String)
assemble = runParserT cinstruction () "hack"

--line :: ParsecT String () Maybe String
--line = ((spaces *> lift Nothing) <|> instruction) <* comment
--
--comment :: Monad m => ParsecT String () m ()
--comment = void $ Parsec.string "//" *> manyTill anyChar eof

cinstruction :: Monad m => ParsecT String () m String
cinstruction = do
  d <- dest <* char '=' <|> return "000"
  c <- comp
  j <- char ';' *> jump <|> return "000"
  return $ "111" <> c <> d <> j

dest :: Monad m => ParsecT String () m String
dest = 
  "001" <$ char 'M' <|>
  "010" <$ char 'D'

comp :: Monad m => ParsecT String () m String
comp = "110000" <$ char 'A'

jump :: Monad m => ParsecT String () m String
jump = "001" <$ Parsec.string "JGT"
