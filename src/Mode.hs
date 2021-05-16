module Mode (modeFromArgs, Mode (Next, Previous)) where

data Mode = Next | Previous deriving (Show)

type Argument = String

modeFromArgs :: [Argument] -> Mode
modeFromArgs arguments
  | "previous" `elem` arguments = Previous
  | otherwise = Next