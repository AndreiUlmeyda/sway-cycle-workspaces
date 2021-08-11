module Main where

import Data.Text (replace, unpack)
import Errors (ErrorMessage (ErrorMessage))
import InputValidation (parseInput)
import Mode (parseArgumentsAndProvideHelpText)
import NewWorkspace (changeWorkspace)
import Turtle (ExitCode, Line, Shell, Text, empty, shellStrict, textToLine, unsafeTextToLine, view)
import Types (WorkspaceIndex (WorkspaceIndex))

main :: IO ()
main = do
  mode <- parseArgumentsAndProvideHelpText
  view $ fmap (unpackResultOrAbort . changeWorkspace mode . parseInput mode . unpack . snd) (preformat =<< getWorkspaceDescription)

unpackResultOrAbort :: Either ErrorMessage WorkspaceIndex -> String
unpackResultOrAbort (Left (ErrorMessage errorMsg)) = error errorMsg
unpackResultOrAbort (Right (WorkspaceIndex index)) = index

getWorkspaceDescription :: Shell (ExitCode, Text)
getWorkspaceDescription = shellStrict "swaymsg --raw --type get_workspaces" empty

preformat :: (ExitCode, Text) -> Shell (ExitCode, Text)
preformat input = shellStrict "./bin/json-to-workspace-lines.jq" (workspaceDescriptionFrom input)

workspaceDescriptionFrom :: (ExitCode, Text) -> Shell Line
workspaceDescriptionFrom = pure . textToLine' . replace "\n" "" . snd

-- TODO properly handle each case
textToLine' :: Text -> Line
textToLine' input
  | Nothing <- input' = unsafeTextToLine "derp"
  | Just line <- input' = line
  where
    input' = textToLine input
