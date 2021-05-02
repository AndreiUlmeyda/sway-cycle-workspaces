module Types (InputLine, Mode (Up, Down)) where

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data Mode = Up | Down

type InputLine = String

type WorkspaceNumber = String

data WorkspaceDescription = WorkspaceDescription String

data Workspace = Workspace
  { output :: String,
    workspaceIndex :: Int,
    focused :: Bool
  }
  deriving (Show)