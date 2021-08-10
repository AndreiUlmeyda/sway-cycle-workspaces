{-# LANGUAGE OverloadedStrings #-}

module Main where

import Errors (ErrorMessage (ErrorMessage))
import Types (WorkspaceIndex (WorkspaceIndex))
import Turtle (Shell, Line, ExitCode, Text, textToLine, view, shellStrict, empty, unsafeTextToLine)
import Data.Text (replace)
import Mode (parseArgumentsAndProvideHelpText)

main :: IO ()
main = do
  mode <- parseArgumentsAndProvideHelpText
  view (preformat =<< getWorkspaceDescription)

getWorkspaceDescription :: Shell (ExitCode, Text)
getWorkspaceDescription = shellStrict "swaymsg --raw --type get_workspaces" empty

preformat :: (ExitCode, Text) -> Shell (ExitCode, Text)
preformat input = shellStrict "./bin/json-to-workspace-lines.jq" (workspaceDescriptionFrom input)

workspaceDescriptionFrom :: (ExitCode, Text) -> Shell Line
workspaceDescriptionFrom= pure . textToLine' . replace "\n" "" .snd

textToLine' :: Text -> Line
textToLine' input
  | Nothing <- input' = unsafeTextToLine "derp"
  | Just line <- input' = line
  where input' = textToLine input

--main = do
--  mode <- parseArgumentsAndProvideHelpText
--  input <- fmap (parseInput mode) getContents
--  printResultOrAbort (changeWorkspace mode input)

printResultOrAbort :: Either ErrorMessage WorkspaceIndex -> IO ()
printResultOrAbort (Left (ErrorMessage errorMessage)) = error errorMessage
printResultOrAbort (Right (WorkspaceIndex workspaceIndex)) = print workspaceIndex
