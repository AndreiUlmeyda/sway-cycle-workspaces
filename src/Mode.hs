module Mode
  ( modeFromArgs,
    Mode (Next, Previous),
  )
where

import Errors (ErrorMessage, errorIncorrectCommandLineArgument)

data Mode = Next | Previous deriving (Show, Eq)

type Argument = String

modeFromArgs :: [Argument] -> Either ErrorMessage Mode
modeFromArgs arguments
  | "previous" `elem` arguments = Right Previous
  | "next" `elem` arguments = Right Next
  | otherwise = Left errorIncorrectCommandLineArgument
