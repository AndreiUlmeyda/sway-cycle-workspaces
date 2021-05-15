module Lib (nextWorkspace, newNextWorkspace, next, previous, ErrorMessage (ErrorMessage), WorkspaceIndex (WorkspaceIndex)) where

import Data.Either
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

newtype ErrorMessage = ErrorMessage String deriving (Show, Eq)

newtype WorkspaceIndex = WorkspaceIndex String deriving (Show, Eq)

newNextWorkspace :: Mode -> [String] -> Either ErrorMessage WorkspaceIndex
newNextWorkspace _ workspaces
  | length workspaces <= 1 = Left (ErrorMessage "Only zero or one lines/workspaces provided. There is nothing to do.")
  | any otherThanThreeWordsLong workspaces = Left (ErrorMessage "Each input line must contain 3 items following the pattern 'output workspacename focused' where focused is either 'true' or 'false'")
  | otherwise = Right $ WorkspaceIndex $ (head . tail . words) (workspaces !! 1)
  where
    otherThanThreeWordsLong = (/= 3) . length . words