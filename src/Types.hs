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

newtype WorkspaceIndex = WorkspaceIndex String deriving stock (Show, Eq)

type WorkspaceFocus = String

type WorkspaceDescriptionPart = String
