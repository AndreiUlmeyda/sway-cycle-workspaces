module Main where

import Control.Monad (void)
import System.Environment (getArgs)
import System.Process (readProcess)
import Types (InputLine, Mode (Down, Up), Workspace (..), WorkspaceDescription (..), WorkspaceNumber)

main :: IO ()
main = do
  mode <- fmap modeFromArgs getArgs
  input <- fmap lines getContents
  void $ readProcess "swaymsg" ["workspace", "number", nextWorkspace mode input] swayMsgStdIn

swayMsgStdIn :: String
swayMsgStdIn = ""

workspaceDescription :: String -> WorkspaceDescription
workspaceDescription = WorkspaceDescription

nextWorkspace :: Mode -> [InputLine] -> WorkspaceNumber
nextWorkspace Up input = (show . next . onlyFocusedOutput) workspaces
  where
    workspaces = map parseWorkspace input
nextWorkspace Down input = (show . previous . onlyFocusedOutput) workspaces
  where
    workspaces = map parseWorkspace input

next :: [Workspace] -> Int
next _ = 4

previous :: [Workspace] -> Int
previous _ = 1

onlyFocusedOutput :: [Workspace] -> [Workspace]
onlyFocusedOutput _ = []

parseWorkspace :: String -> Workspace
parseWorkspace inputLine = Workspace {output = output, workspaceIndex = workspaceIndex, focused = focused}
  where
    items = words inputLine
    output = head items
    workspaceIndex = (read . head . tail) items :: Int
    focused = last items == "true"

modeFromArgs :: a -> Mode
modeFromArgs = const Up