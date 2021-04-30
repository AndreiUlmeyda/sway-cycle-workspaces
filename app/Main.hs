module Main where

import Lib (cycleWorkSpaces)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= interact . cycleWorkspacesWithMode . modeFromArgs

data Mode = Up | Down

data Workspace = Workspace
  { output :: String,
    workspaceIndex :: Int,
    focused :: Bool
  }
  deriving (Show)

cycleWorkspacesWithMode :: Mode -> String -> String
cycleWorkspacesWithMode _ input = show $ map (parseWorkspace . words) (lines input)

parseWorkspace :: [String] -> Workspace
parseWorkspace input = Workspace {output = output, workspaceIndex = workspaceIndex, focused = focused}
  where
    output = head input
    workspaceIndex = read (input !! 1) :: Int
    focused = (input !! 2) == "true"

modeFromArgs :: a -> Mode
modeFromArgs = const Up