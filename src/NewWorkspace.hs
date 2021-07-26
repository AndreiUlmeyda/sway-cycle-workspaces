module NewWorkspace
  ( changeWorkspace,
    isFocused,
  )
where

import Errors (ErrorMessage)
import Mode (Mode (Next, Previous))
import Types
  ( WorkspaceDescription,
    WorkspaceDescriptionPart,
    WorkspaceDisplay,
    WorkspaceFocus,
    WorkspaceIndex (WorkspaceIndex),
  )

changeWorkspace :: Either ErrorMessage Mode -> Either ErrorMessage [WorkspaceDescription] -> Either ErrorMessage WorkspaceIndex
changeWorkspace mode workspaces
  | Left errorMsg <- mode = Left errorMsg
  | Left errorMsg <- workspaces = Left errorMsg
  | Right validMode <- mode,
    Right validWorkspaces <- workspaces =
    Right (changeWorkspace' validMode validWorkspaces)

changeWorkspace' :: Mode -> [WorkspaceDescription] -> WorkspaceIndex
changeWorkspace' mode workspaces
  | Previous <- mode = (getWorkspaceIndex . last) (takeWhile (not . isFocused) workspacesFromFocusedDisplay)
  | Next <- mode = (getWorkspaceIndex . secondElement) (dropWhile (not . isFocused) workspacesFromFocusedDisplay)
  where
    workspacesFromFocusedDisplay = onlyFocusedDisplay workspaces

onlyFocusedDisplay :: [WorkspaceDescription] -> [WorkspaceDescription]
onlyFocusedDisplay workspaces = filter isFocusedDisplay workspaces
  where
    isFocusedDisplay workspace = getDisplay workspace == (getDisplay . head) (filter isFocused workspaces)

isFocused :: WorkspaceFocus -> Bool
isFocused = (== "true") . thirdElement . words

getDisplay :: WorkspaceDescription -> WorkspaceDisplay
getDisplay = head . words

getWorkspaceIndex :: WorkspaceDescription -> WorkspaceIndex
getWorkspaceIndex = WorkspaceIndex . secondElement . words

secondElement :: [WorkspaceDescriptionPart] -> WorkspaceDescriptionPart
secondElement = (!! 1)

thirdElement :: [WorkspaceDescriptionPart] -> WorkspaceDescriptionPart
thirdElement = (!! 2)
