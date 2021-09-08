module Main where

import Errors (descriptionRetrievalError, formatConversionError, reportShellCommandErrors, workspaceChangeError)
import ExternalCommands (getWorkspaceDescriptionJson, jsonToLineFormat, switchToWorkspace)
import InputValidation (parseInput)
import Mode (parseArgumentsAndProvideHelpText)
import NewWorkspace (determineWorkspaceNumber)
import Turtle (view)

main :: IO ()
main = do
  mode <- parseArgumentsAndProvideHelpText
  let input = parseInput mode
  view
    ( reportShellCommandErrors workspaceChangeError
        =<< switchToWorkspace
        =<< determineWorkspaceNumber mode input
        =<< reportShellCommandErrors formatConversionError
        =<< jsonToLineFormat
        =<< reportShellCommandErrors descriptionRetrievalError
        =<< getWorkspaceDescriptionJson
    )
