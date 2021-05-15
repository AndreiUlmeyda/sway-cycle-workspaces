module Mode (modeFromArgs, Mode (Next, Previous)) where

data Mode = Next | Previous

type Argument = String

modeFromArgs :: [Argument] -> Mode
modeFromArgs arguments
  | "previous" `elem` arguments = Previous
  | otherwise = Next