module Main where

import Lib (changeWorkspace)
import Mode (Mode, modeFromArgs)
import System.Environment (getArgs)
import Types (InputLine)

main :: IO ()
main = do
  mode <- fmap modeFromArgs getArgs
  input <- fmap lines getContents
  print (changeWorkspace mode input)