module NewWorkspace (changeWorkspace, WorkspaceIndex (WorkspaceIndex), isFocused) where

import Data.Either ()
import Errors
  ( ErrorMessage,
  )
import Mode (Mode (Next, Previous))

newtype WorkspaceIndex = WorkspaceIndex String deriving (Show, Eq)

changeWorkspace :: Either ErrorMessage Mode -> Either ErrorMessage [String] -> Either ErrorMessage WorkspaceIndex
changeWorkspace mode workspaces
  | Left errorMsg <- mode = Left errorMsg
  | Left errorMsg <- workspaces = Left errorMsg
  | Right validMode <- mode,
    Right validWorkspaces <- workspaces
    = changeWorkspace' validMode validWorkspaces

changeWorkspace' :: Mode -> [String] -> Either a WorkspaceIndex
changeWorkspace' mode workspaces
  | Previous <- mode = (markAsResult . last) (takeWhile (not . isFocused) workspacesFromFocusedOutput)
  | Next <- mode = (markAsResult . secondElement) (dropWhile (not . isFocused) workspacesFromFocusedOutput)
  where workspacesFromFocusedOutput = onlyFocusedOutput workspaces
        markAsResult = Right . WorkspaceIndex . getWorkspaceIndex

onlyFocusedOutput :: [String] -> [String]
onlyFocusedOutput workspaces = filter isFocusedOutput workspaces
  where
    isFocusedOutput workspace = getOutput workspace == (getOutput . head) (filter isFocused workspaces)

isFocused :: String -> Bool
isFocused = (== "true") . getIsFocused

getIsFocused :: String -> String
getIsFocused = thirdElement . words

-- TODO use safe variants of prelude functions
getOutput :: String -> String
getOutput = head . words

getWorkspaceIndex :: String -> String
getWorkspaceIndex = secondElement . words

secondElement :: [a] -> a
secondElement = (!! 1)

thirdElement :: [a] -> a
thirdElement = (!! 2)