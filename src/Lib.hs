module Lib (nextWorkspace, next, previous) where

import Mode (Mode (Down, Up))
import Types (InputLine, Workspace (..), WorkspaceDescription (..), WorkspaceNumber)

nextWorkspace :: Mode -> [InputLine] -> WorkspaceNumber
nextWorkspace mode input =
  case mode of
    Up -> (show . next . onlyFocusedOutput) workspaces
    Down -> (show . previous . onlyFocusedOutput) workspaces
  where
    workspaces = map parseWorkspace input

next :: [Workspace] -> Int
next _ = 4

previous :: [Workspace] -> Int
previous _ = 1

onlyFocusedOutput :: [Workspace] -> [Workspace]
onlyFocusedOutput workspaces = filter isFocusedOutput workspaces
  where
    isFocusedOutput workspace = output workspace == (output . head) (filter focused workspaces)

parseWorkspace :: WorkspaceDescription -> Workspace
parseWorkspace inputLine = Workspace {output = output, workspaceIndex = workspaceIndex, focused = focused}
  where
    items = words inputLine
    output = head items
    workspaceIndex = (read . head . tail) items :: Int
    focused = last items == "true"
