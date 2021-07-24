module InputValidation (parseInput) where

import Errors
  ( ErrorMessage
  , errorTooFewInputWorkspaces
  , errorWrongInputLayout
  , errorNotExactlyOneFocusedWorkspace
  , errorNoPreviousWorkspace
  , errorNoNextWorkspace)
import Mode (Mode(Next, Previous))

-- TODO move deleted unit tests for lib to this module
parseInput :: Either ErrorMessage Mode -> String -> Either ErrorMessage [String]
parseInput mode input
  -- error out if relevant inputs are empty
 | length workspaces <= 1 = Left errorTooFewInputWorkspaces
 | any notThreeWordsLong workspaces = Left errorWrongInputLayout
 -- assume a single focused workspace
 | length (filter isFocused workspaces) /= 1 = Left errorNotExactlyOneFocusedWorkspace
 | Right Next <- mode,
     (isFocused . last) workspaces =
     Left errorNoNextWorkspace
 -- catch cases where there is no next/previous workspace
 | Right Previous <- mode,
   (isFocused . head) workspaces =
   Left errorNoPreviousWorkspace
 | otherwise = Right workspaces
  where
    workspaces = lines input

-- TODO resolve duplicate code between this module and lib
-- TODO use safe variants of prelude functions
notThreeWordsLong :: String -> Bool
notThreeWordsLong = (/= 3) . length . words

isFocused :: String -> Bool
isFocused = (== "true") . getIsFocused

getIsFocused :: String -> String
getIsFocused = thirdElement . words

thirdElement :: [a] -> a
thirdElement = (!! 2)