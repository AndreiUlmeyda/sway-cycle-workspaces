module Main where

import Data.Text (append, pack, replace, unpack)
import Errors (descriptionRetrievalError, formatConversionError, reportShellCommandErrors, workspaceChangeError)
import InputValidation (parseInput)
import Mode (Mode, parseArgumentsAndProvideHelpText)
import NewWorkspace (newWorkspaceNumber)
import Turtle (ExitCode, Line, Shell, Text, empty, shellStrict, textToLine, view)
import Types (WorkspaceIndex (WorkspaceIndex))

main :: IO ()
main = do
  mode <- parseArgumentsAndProvideHelpText
  view
    ( reportShellCommandErrors workspaceChangeError
        =<< switchToWorkspace
        =<< determineWorkspaceNumber mode
        =<< reportShellCommandErrors formatConversionError
        =<< jsonToLineFormat
        =<< reportShellCommandErrors descriptionRetrievalError
        =<< getWorkspaceDescriptionJson
    )

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

toLine :: Text -> Line
toLine input
  | Nothing <- input' = error "The workspace description obtained from swaymsg couldn't be parsed as lines of text"
  | Just line <- input' = line
  where
    input' = textToLine input

determineWorkspaceNumber :: Mode -> Text -> Shell String
determineWorkspaceNumber mode = pure . fromWorkspaceIndex . newWorkspaceNumber mode . parseInput mode . unpack

-- TODO implement show correctly or move this to the respective type declaration
fromWorkspaceIndex :: WorkspaceIndex -> String
fromWorkspaceIndex (WorkspaceIndex index) = index
