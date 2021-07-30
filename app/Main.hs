module Main where

import Errors (ErrorMessage (ErrorMessage))
import InputValidation (parseInput)
import Mode (parseArgumentsAndProvideHelpText)
import NewWorkspace (changeWorkspace)
import Types (WorkspaceIndex (WorkspaceIndex))

main :: IO ()
main = do
  mode <- parseArgumentsAndProvideHelpText
  input <- fmap (parseInput mode) getContents
  printResultOrAbort (changeWorkspace mode input)

printResultOrAbort :: Either ErrorMessage WorkspaceIndex -> IO ()
printResultOrAbort (Left (ErrorMessage errorMessage)) = error errorMessage
printResultOrAbort (Right (WorkspaceIndex workspaceIndex)) = print workspaceIndex
