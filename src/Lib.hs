module Lib (changeWorkspace, WorkspaceIndex (WorkspaceIndex)) where

import Data.Either ()
import Errors (ErrorMessage, errorTooFewInputWorkspaces, errorWrongInputLayout)
import Mode (Mode (Next, Previous))
import Types (InputLine, Workspace (..), WorkspaceDescription (..), WorkspaceNumber)

-- onlyFocusedOutput :: [Workspace] -> [Workspace]
-- onlyFocusedOutput workspaces = filter isFocusedOutput workspaces
--   where
--     isFocusedOutput workspace = output workspace == (output . head) (filter focused workspaces)

newtype WorkspaceIndex = WorkspaceIndex String deriving (Show, Eq)

changeWorkspace :: Mode -> [String] -> Either ErrorMessage WorkspaceIndex
changeWorkspace _ workspaces
  | length workspaces <= 1 = Left errorTooFewInputWorkspaces
  | any otherThanThreeWordsLong workspaces = Left errorWrongInputLayout
  | otherwise = Right $ WorkspaceIndex $ (head . tail . words) (workspaces !! 1)
  where
    otherThanThreeWordsLong = (/= 3) . length . words