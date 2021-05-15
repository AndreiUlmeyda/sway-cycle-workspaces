module Lib (changeWorkspace, WorkspaceIndex (WorkspaceIndex)) where

import Data.Either ()
import Errors (ErrorMessage, errorNoNextWorkspace, errorNoPreviousWorkspace, errorNotExactlyOneFocusedWorkspace, errorTooFewInputWorkspaces, errorWrongInputLayout)
import Mode (Mode (Next, Previous))
import Types (InputLine, Workspace (..), WorkspaceDescription (..), WorkspaceNumber)

-- onlyFocusedOutput :: [Workspace] -> [Workspace]
-- onlyFocusedOutput workspaces = filter isFocusedOutput workspaces
--   where
--     isFocusedOutput workspace = output workspace == (output . head) (filter focused workspaces)

newtype WorkspaceIndex = WorkspaceIndex String deriving (Show, Eq)

changeWorkspace :: Mode -> [String] -> Either ErrorMessage WorkspaceIndex
changeWorkspace mode workspaces
  | length workspaces <= 1 = Left errorTooFewInputWorkspaces
  | any otherThanThreeWordsLong workspaces = Left errorWrongInputLayout
  | length (filter isFocused workspaces) /= 1 = Left errorNotExactlyOneFocusedWorkspace
  | Next <- mode,
    (isFocused . last) workspaces =
    Left errorNoNextWorkspace
  | Previous <- mode,
    (isFocused . head) workspaces =
    Left errorNoPreviousWorkspace
  | otherwise = Right $ WorkspaceIndex $ (head . tail . words) (workspaces !! 1)
  where
    otherThanThreeWordsLong = (/= 3) . length . words
    isFocused = (== "true") . (!! 2) . words