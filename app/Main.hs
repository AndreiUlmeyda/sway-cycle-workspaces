module Main where

import Data.Text (append, pack, replace, unpack)
import Errors (ErrorMessage (ErrorMessage))
import InputValidation (parseInput)
import Mode (Mode, parseArgumentsAndProvideHelpText)
import NewWorkspace (newWorkspaceNumber)
import Turtle (ExitCode (ExitSuccess), Line, Shell, Text, empty, shellStrict, textToLine, view)
import Types (WorkspaceIndex (WorkspaceIndex))

main :: IO ()
main = do
  mode <- parseArgumentsAndProvideHelpText
  view
    ( switchToWorkspace
        =<< determineWorkspaceNumber mode
        =<< reportErrors formatConversionError
        =<< jsonToLineFormat
        =<< reportErrors descriptionRetrievalError
        =<< getWorkspaceDescriptionJson
    )

reportErrors :: String -> (ExitCode, Text) -> Shell Text
reportErrors errorDescription (exitCode, resultText)
  | exitCode == ExitSuccess = pure resultText
  | otherwise = error (errorDescription ++ " Exitcode: " ++ show exitCode)

descriptionRetrievalError :: String
descriptionRetrievalError = "Tried to execute 'swaymsg' to retrieve a workspace layout but the command failed."

formatConversionError :: String
formatConversionError = "Tried to execute 'jq' to reformat the output of 'swaymsg' but the command failed."

switchToWorkspace :: String -> Shell (ExitCode, Text)
switchToWorkspace workspaceIndex = shellStrict (append "swaymsg workspace number " (pack workspaceIndex)) empty

getWorkspaceDescriptionJson :: Shell (ExitCode, Text)
getWorkspaceDescriptionJson = shellStrict "swaymsg --raw --type get_workspaces" empty

jsonToLineFormat :: Text -> Shell (ExitCode, Text)
jsonToLineFormat = shellStrict "/usr/lib/sway-cycle-workspaces/json-to-workspace-lines.jq" . resultAsLinesWithoutNewlines

resultAsLinesWithoutNewlines :: Text -> Shell Line
resultAsLinesWithoutNewlines = pure . toLine . stripNewLines

stripNewLines :: Text -> Text
stripNewLines = replace "\n" ""

-- TODO align error handling with the rest of the application
toLine :: Text -> Line
toLine input
  | Nothing <- input' = error "The workspace description obtained from swaymsg couldn't be parsed as lines of text"
  | Just line <- input' = line
  where
    input' = textToLine input

determineWorkspaceNumber :: Mode -> Text -> Shell String
determineWorkspaceNumber mode = unpackResultOrAbort . newWorkspaceNumber mode . parseInput mode . unpack

unpackResultOrAbort :: Either ErrorMessage WorkspaceIndex -> Shell String
unpackResultOrAbort (Left (ErrorMessage errorMsg)) = error errorMsg
unpackResultOrAbort (Right (WorkspaceIndex index)) = pure index
