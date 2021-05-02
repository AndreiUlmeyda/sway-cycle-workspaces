module Mode (modeFromArgs, Mode (Up, Down)) where

data Mode = Up | Down

modeFromArgs :: a -> Mode
modeFromArgs = const Up