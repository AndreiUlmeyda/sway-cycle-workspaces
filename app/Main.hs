module Main where

import Control.Monad (void)
import Lib (nextWorkspace)
import Mode (Mode, modeFromArgs)
import System.Environment (getArgs)
import System.Process (readProcess)
import Types (InputLine)

main :: IO ()
main = do
  mode <- fmap modeFromArgs getArgs
  input <- fmap lines getContents
  changeWorkspaceUpOrDown mode input

changeWorkspaceUpOrDown :: Mode -> [InputLine] -> IO ()
changeWorkspaceUpOrDown mode input
  | all null input = print "Error: A list of workspaces needs to be provided as input."
  | otherwise = void $ readProcess "swaymsg" ["workspace", "number", nextWorkspace mode input] swayMsgStdIn

-- the swaymsg command is determined by its arguments, nothing is piped in
swayMsgStdIn :: String
swayMsgStdIn = ""