module Lib (changeWorkspace, WorkspaceIndex (WorkspaceIndex)) where

import Data.Either ()
import Errors
  ( ErrorMessage,
    errorNoNextWorkspace,
    errorNoPreviousWorkspace,
    errorNotExactlyOneFocusedWorkspace,
    errorTooFewInputWorkspaces,
    errorUnexpectedInput,
    errorWrongInputLayout,
  )
import Mode (Mode (Next, Previous))
import Types (InputLine, Workspace (..), WorkspaceDescription (..), WorkspaceNumber)

newtype WorkspaceIndex = WorkspaceIndex String deriving (Show, Eq)

changeWorkspace :: Mode -> [String] -> Either ErrorMessage WorkspaceIndex
changeWorkspace mode workspaces
  -- error out if relevant inputs are empty
  | length workspaces <= 1 = Left errorTooFewInputWorkspaces
  | any notThreeWordsLong workspaces = Left errorWrongInputLayout
  -- assume a single focused workspace
  | length (filter isFocused workspaces) /= 1 = Left errorNotExactlyOneFocusedWorkspace
  -- catch cases where there is no next/previous workspace
  | Next <- mode,
    (isFocused . last) workspaces =
    Left errorNoNextWorkspace
  | Previous <- mode,
    (isFocused . head) workspaces =
    Left errorNoPreviousWorkspace
  -- determine the new workspace index
  | Previous <- mode = (Right . WorkspaceIndex . getWorkspaceIndex . last) (takeWhile (not . isFocused) workspacesFromFocusedOutput)
  | Next <- mode = (Right . WorkspaceIndex . getWorkspaceIndex . secondElement) (dropWhile (not . isFocused) workspacesFromFocusedOutput)
  | otherwise = Left errorUnexpectedInput
  where
    workspacesFromFocusedOutput = onlyFocusedOutput workspaces

onlyFocusedOutput :: [String] -> [String]
onlyFocusedOutput workspaces = filter isFocusedOutput workspaces
  where
    isFocusedOutput workspace = getOutput workspace == (getOutput . head) (filter isFocused workspaces)

notThreeWordsLong :: String -> Bool
notThreeWordsLong = (/= 3) . length . words

isFocused :: String -> Bool
isFocused = (== "true") . getIsFocused

getOutput :: String -> String
getOutput = head . words

getWorkspaceIndex :: String -> String
getWorkspaceIndex = secondElement . words

getIsFocused :: String -> String
getIsFocused = thirdElement . words

secondElement :: [a] -> a
secondElement = (!! 1)

thirdElement :: [a] -> a
thirdElement = (!! 2)
