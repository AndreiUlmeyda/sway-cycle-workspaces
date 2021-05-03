module Mode (modeFromArgs, Mode (Next, Previous)) where

data Mode = Next | Previous

modeFromArgs :: a -> Mode
modeFromArgs = const Next