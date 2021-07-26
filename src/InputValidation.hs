module InputValidation (parseInput) where

import Errors
  ( ErrorMessage,
    errorNoNextWorkspace,
    errorNoPreviousWorkspace,
    errorNotExactlyOneFocusedWorkspace,
    errorTooFewInputWorkspaces,
    errorWrongInputLayout,
  )
import Mode (Mode (Next, Previous))
import NewWorkspace (isFocused)

parseInput :: Either ErrorMessage Mode -> String -> Either ErrorMessage [String]
parseInput mode input
  -- error out if relevant inputs are empty
  | length workspaces <= 1 = Left errorTooFewInputWorkspaces
  | any notThreeWordsLong workspaces = Left errorWrongInputLayout
  -- assume a single focused workspace
  | length (filter isFocused workspaces) /= 1 = Left errorNotExactlyOneFocusedWorkspace
  -- catch cases where there is no next/previous workspace
  | Right Next <- mode,
    (isFocused . last) workspaces =
    Left errorNoNextWorkspace
  | Right Previous <- mode,
    (isFocused . head) workspaces =
    Left errorNoPreviousWorkspace
  | otherwise = Right workspaces
  where
    workspaces = lines input

notThreeWordsLong :: String -> Bool
notThreeWordsLong = (/= 3) . length . words
