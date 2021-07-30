module Errors
  ( ErrorMessage (ErrorMessage),
    errorTooFewInputWorkspaces,
    errorWrongInputLayout,
    errorNoNextWorkspace,
    errorNoPreviousWorkspace,
    errorNotExactlyOneFocusedWorkspace,
    errorUnexpectedInput,
  )
where

newtype ErrorMessage = ErrorMessage String deriving (Show, Eq)

errorTooFewInputWorkspaces :: ErrorMessage
errorTooFewInputWorkspaces = ErrorMessage "Only zero or one lines/workspaces provided. There is nothing to do, exiting..."

errorWrongInputLayout :: ErrorMessage
errorWrongInputLayout = ErrorMessage "Each input line must contain 3 items following the pattern 'output workspacename focused' where focused is either 'true' or 'false'. Exiting..."

errorNoNextWorkspace :: ErrorMessage
errorNoNextWorkspace = ErrorMessage "There is no next workspace since the last one is focused. Exiting..."

errorNoPreviousWorkspace :: ErrorMessage
errorNoPreviousWorkspace = ErrorMessage "There is no previous workspace since the first one is focused. Exiting..."

errorNotExactlyOneFocusedWorkspace :: ErrorMessage
errorNotExactlyOneFocusedWorkspace = ErrorMessage "There should only be exactly one focused workspace. Exiting..."

errorUnexpectedInput :: ErrorMessage
errorUnexpectedInput = ErrorMessage "This error is not supposed to ever occur. If it nevertheless does, I would be grateful if you found the time to file a bug report, if possible, supplying the input which caused it."
