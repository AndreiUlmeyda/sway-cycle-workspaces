{-# LANGUAGE OverloadedStrings #-}

module Main where

import Errors (ErrorMessage (ErrorMessage))
import InputValidation (parseInput)
import Mode (parseArgumentsAndProvideHelpText)
import NewWorkspace (changeWorkspace)
import Types (WorkspaceIndex (WorkspaceIndex))
import Turtle (textToLines, Shell, Line, ExitCode, Text, textToLine, view, shellStrict, empty, unsafeTextToLine)
import Data.Maybe (fromJust, Maybe)
import Data.Text (replace)

main :: IO ()
main = view (preformat =<< getWorkspaceDescription)

--workspaceDescriptionJson :: Shell Line
--workspaceDescriptionJson = fromJust . textToLine <$> fmap snd getWorkspaceDescription

getWorkspaceDescription :: Shell (ExitCode, Text)
getWorkspaceDescription = shellStrict "swaymsg --raw --type get_workspaces" empty

preformat :: (ExitCode, Text) -> Shell (ExitCode, Text)
preformat input = shellStrict "./json-to-workspace-lines.jq" (workspaceDescriptionFrom input)

workspaceDescriptionFrom :: (ExitCode, Text) -> Shell Line
workspaceDescriptionFrom= pure . textToLine' . (replace "\n" "") .snd

textToLine' :: Text -> Line
textToLine' input
  | Nothing <- input' = unsafeTextToLine "derp"
  | Just line <- input' = line
  where input' = textToLine input

--  |Text
--main = do
--  mode <- parseArgumentsAndProvideHelpText
--  input <- fmap (parseInput mode) getContents
--  printResultOrAbort (changeWorkspace mode input)

printResultOrAbort :: Either ErrorMessage WorkspaceIndex -> IO ()
printResultOrAbort (Left (ErrorMessage errorMessage)) = error errorMessage
printResultOrAbort (Right (WorkspaceIndex workspaceIndex)) = print workspaceIndex
