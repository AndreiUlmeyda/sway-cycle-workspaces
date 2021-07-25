module Main where

import Errors (ErrorMessage (ErrorMessage))
import InputValidation (parseInput)
import NewWorkspace (WorkspaceIndex (WorkspaceIndex), changeWorkspace)
import Mode (modeFromArgs)
import System.Environment (getArgs)

main :: IO ()
main = do
  mode <- fmap modeFromArgs getArgs
  input <- fmap (parseInput mode) getContents
  printOutput (changeWorkspace mode input)

printOutput :: Either ErrorMessage WorkspaceIndex -> IO ()
printOutput (Left (ErrorMessage errorMessage)) = error errorMessage
printOutput (Right (WorkspaceIndex workspaceIndex)) = print workspaceIndex