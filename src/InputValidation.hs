module InputValidation (parseInput) where

import Errors
  ( errorNoNextWorkspace,
    errorNoPreviousWorkspace,
    errorNotExactlyOneFocusedWorkspace,
    errorTooFewInputWorkspaces,
    errorWrongInputLayout,
  )
import Mode (Mode (Next, Previous))
import NewWorkspace (isFocused)

parseInput :: Mode -> String -> [String]
parseInput mode input
  -- error out if relevant inputs are empty
  | length workspaces <= 1 = error (show errorTooFewInputWorkspaces)
  | any notThreeWordsLong workspaces = error (show errorWrongInputLayout)
  -- assume a single focused workspace
  | length (filter isFocused workspaces) /= 1 = error (show errorNotExactlyOneFocusedWorkspace)
  -- catch cases where there is no next/previous workspace
  | Next <- mode,
    (isFocused . last) workspaces =
    error (show errorNoNextWorkspace)
  | Previous <- mode,
    (isFocused . head) workspaces =
    error (show errorNoPreviousWorkspace)
  | otherwise = workspaces
  where
    workspaces = lines input

notThreeWordsLong :: String -> Bool
notThreeWordsLong = (/= 3) . length . words
