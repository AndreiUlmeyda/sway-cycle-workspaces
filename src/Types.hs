module Types (WorkspaceDescription (..), InputLine, WorkspaceNumber, Workspace (..)) where

type InputLine = String

type WorkspaceNumber = String

type WorkspaceDescription = String

data Workspace = Workspace
  { output :: String,
    workspaceIndex :: Int,
    focused :: Bool
  }
  deriving (Show)