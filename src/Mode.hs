module Mode (modeFromArgs, Mode (Next, Previous)) where

data Mode = Next | Previous

type Argument = String

modeFromArgs :: [Argument] -> Mode
modeFromArgs arguments
  | "-d" `elem` arguments = Previous
  | otherwise = Next