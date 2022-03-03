module Errors
  ( ErrorMessage (ErrorMessage),
    errorTooFewInputWorkspaces,
    errorWrongInputLayout,
    errorNoNextWorkspace,
    errorNoPreviousWorkspace,
    errorNotExactlyOneFocusedWorkspace,
    descriptionRetrievalError,
    formatConversionError,
    reportShellCommandErrors,
    workspaceChangeError,
  )
where

import Data.Text (Text)
import Turtle (ExitCode (ExitSuccess), Shell)
import Prelude

newtype ErrorMessage = ErrorMessage String deriving stock (Show, Eq)

errorTooFewInputWorkspaces :: ErrorMessage
errorTooFewInputWorkspaces = ErrorMessage "Sway (swaymsg) reported only or zero workspaces. There is nothing to do, exiting..."

errorWrongInputLayout :: ErrorMessage
errorWrongInputLayout = ErrorMessage "Each input line must contain 3 items following the pattern 'output workspacename focused' where focused is either 'true' or 'false'. Exiting..."

errorNoNextWorkspace :: ErrorMessage
errorNoNextWorkspace = ErrorMessage "There is no next workspace since the last one is focused. Exiting..."

errorNoPreviousWorkspace :: ErrorMessage
errorNoPreviousWorkspace = ErrorMessage "There is no previous workspace since the first one is focused. Exiting..."

errorNotExactlyOneFocusedWorkspace :: ErrorMessage
errorNotExactlyOneFocusedWorkspace = ErrorMessage "There should only be exactly one focused workspace. Exiting..."

descriptionRetrievalError :: ErrorMessage
descriptionRetrievalError = ErrorMessage "Tried to execute 'swaymsg' to retrieve a workspace layout but the command failed."

formatConversionError :: ErrorMessage
formatConversionError = ErrorMessage "Tried to execute 'jq' to reformat the output of 'swaymsg' but the command failed."

workspaceChangeError :: ErrorMessage
workspaceChangeError = ErrorMessage "Tried to execute 'swaymsg' to change the workspace but the command failed."

reportShellCommandErrors :: ErrorMessage -> (ExitCode, Text) -> Shell Text
reportShellCommandErrors errorDescription (exitCode, resultText)
  | exitCode == ExitSuccess = pure resultText
  | otherwise = error (show errorDescription ++ " Exitcode: " ++ show exitCode)
