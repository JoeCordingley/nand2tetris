module Lib
    ( printFile
    , InputFile(..)
    , OutputFile(..)
    ) where

import Pipes
import qualified Pipes.Text.IO as Text
import Pipes.Safe

data InputFile = InputFile String
data OutputFile = OutputFile String

printFile :: InputFile -> OutputFile -> IO ()
printFile (InputFile input) (OutputFile output) = runSafeT $ runEffect $ Text.readFile input >-> Text.writeFile output
