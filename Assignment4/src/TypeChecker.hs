

-- you may replace this file with your implementation of TypeChecker.hs from the previous assignment or just keep it as it is --


module TypeChecker ( typecheck ) where

import ErrM( Err(..) )
import AbsCPP ( Program )

typecheck :: Program -> Err ()
typecheck _ = Ok ()