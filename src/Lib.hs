module Lib (nextWorkspace, next, previous) where

import Mode (Mode (Next, Previous))
import Types (InputLine, Workspace (..), WorkspaceDescription (..), WorkspaceNumber)

nextWorkspace :: Mode -> [InputLine] -> WorkspaceNumber
nextWorkspace mode input =
  case mode of
    Next -> changeWorkspace next
    Previous -> changeWorkspace previous
  where
    workspaces = map parseWorkspace input
    changeWorkspace nextOrPrevious = (show . nextOrPrevious . onlyFocusedOutput) workspaces

next :: [Workspace] -> Int
next = workspaceIndex . head . tail . dropWhile (not . focused)

previous :: [Workspace] -> Int
previous = workspaceIndex . last . takeWhile (not . focused)

onlyFocusedOutput :: [Workspace] -> [Workspace]
onlyFocusedOutput workspaces = filter isFocusedOutput workspaces
  where
    isFocusedOutput workspace = output workspace == (output . head) (filter focused workspaces)

parseWorkspace :: WorkspaceDescription -> Workspace
parseWorkspace inputLine =
  Workspace
    { output = head items,
      workspaceIndex = (read . secondItem) items :: Int,
      focused = last items == "true"
    }
  where
    items = words inputLine
    secondItem = head . tail
