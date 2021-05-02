module Main where

import Control.Monad (void)
import Lib (nextWorkspace)
import Mode (modeFromArgs)
import System.Environment (getArgs)
import System.Process (readProcess)

main :: IO ()
main = do
  mode <- fmap modeFromArgs getArgs
  input <- fmap lines getContents
  void $ readProcess "swaymsg" ["workspace", "number", nextWorkspace mode input] swayMsgStdIn

-- the swaymsg command is determined by its arguments, nothing is piped in
swayMsgStdIn :: String
swayMsgStdIn = ""