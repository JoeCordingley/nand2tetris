module Lib
    ( printFile
    , InputFile(..)
    , OutputFile(..)
    , assemble
    ) where

import Pipes
import Pipes.Text (Text)
import Pipes.Prelude (mapFoldable)
import qualified Pipes.Text.IO as Text
import Pipes.Safe

data InputFile = InputFile String
data OutputFile = OutputFile String

printFile :: InputFile -> OutputFile -> IO ()
printFile (InputFile input) (OutputFile output) = runSafeT $ runEffect $ Text.readFile input >-> mapFoldable assemble >-> Text.writeFile output

assemble :: Text -> [Text]
assemble = undefined
