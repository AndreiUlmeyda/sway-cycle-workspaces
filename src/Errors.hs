module Errors (ErrorMessage, errorTooFewInputWorkspaces, errorWrongInputLayout) where

newtype ErrorMessage = ErrorMessage String deriving (Show, Eq)

errorTooFewInputWorkspaces :: ErrorMessage
errorTooFewInputWorkspaces = ErrorMessage "Only zero or one lines/workspaces provided. There is nothing to do."

errorWrongInputLayout :: ErrorMessage
errorWrongInputLayout = ErrorMessage "Each input line must contain 3 items following the pattern 'output workspacename focused' where focused is either 'true' or 'false'"