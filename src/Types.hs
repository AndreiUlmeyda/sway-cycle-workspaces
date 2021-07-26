module Types
  ( WorkspaceDescription,
    WorkspaceDisplay,
    WorkspaceIndex (WorkspaceIndex),
    WorkspaceFocus,
    WorkspaceDescriptionPart,
  )
where

type WorkspaceDescription = String

type WorkspaceDisplay = String

newtype WorkspaceIndex = WorkspaceIndex String deriving (Show, Eq)

type WorkspaceFocus = String

type WorkspaceDescriptionPart = String
