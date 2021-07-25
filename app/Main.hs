module Main where

import Errors ( ErrorMessage ( ErrorMessage ) )
import InputValidation ( parseInput )
import NewWorkspace ( changeWorkspace )
import Mode ( modeFromArgs )
import System.Environment ( getArgs )
import Types ( WorkspaceIndex ( WorkspaceIndex ) )

main :: IO ()
main = do
  mode <- fmap modeFromArgs getArgs
  input <- fmap (parseInput mode) getContents
  printResultOrAbort (changeWorkspace mode input)

printResultOrAbort :: Either ErrorMessage WorkspaceIndex -> IO ()
printResultOrAbort (Left (ErrorMessage errorMessage)) = error errorMessage
printResultOrAbort (Right (WorkspaceIndex workspaceIndex)) = print workspaceIndex