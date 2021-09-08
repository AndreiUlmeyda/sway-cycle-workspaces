module NewWorkspace
  ( determineWorkspaceNumber,
    newWorkspaceNumber,
    isFocused,
  )
where

import Data.Text (Text, unpack)
import Mode (Mode (Next, Previous))
import Types
  ( WorkspaceDescription,
    WorkspaceDescriptionPart,
    WorkspaceDisplay,
    WorkspaceFocus,
    WorkspaceIndex (WorkspaceIndex),
  )

determineWorkspaceNumber :: Applicative f => Mode -> (String -> [WorkspaceDescription]) -> Text -> f String
determineWorkspaceNumber mode input = pure . show . newWorkspaceNumber mode . input . unpack

newWorkspaceNumber :: Mode -> [WorkspaceDescription] -> WorkspaceIndex
newWorkspaceNumber mode workspaces
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
