module ExternalCommands
  ( switchToWorkspace,
    getWorkspaceDescriptionJson,
    jsonToLineFormat,
  )
where

import Data.Text (Text, append, pack, replace)
import Turtle (ExitCode, shellStrict, empty, Line, textToLine)
import Turtle.Shell (Shell)

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
