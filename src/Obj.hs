module Obj where

{- Parses wavefront .obj files -}
import Types

import Text.Parsec
import Data.Map (Map)
import qualified Data.Map


parseObj :: FilePath -> Map String Object
parseObj = undefined
