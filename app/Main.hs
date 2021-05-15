module Main where

import Errors (ErrorMessage (ErrorMessage))
import Lib (WorkspaceIndex (WorkspaceIndex), changeWorkspace)
import Mode (Mode, modeFromArgs)
import System.Environment (getArgs)
import Types (InputLine, Workspace (workspaceIndex))

main :: IO ()
main = do
  mode <- fmap modeFromArgs getArgs
  input <- fmap lines getContents
  printOutput (changeWorkspace mode input)

printOutput :: Either ErrorMessage WorkspaceIndex -> IO ()
printOutput (Left (ErrorMessage errorMessage)) = error errorMessage
printOutput (Right (WorkspaceIndex workspaceIndex)) = print workspaceIndex