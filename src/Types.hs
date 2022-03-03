module Types
  ( WorkspaceDescription,
    WorkspaceDisplay,
    WorkspaceIndex (WorkspaceIndex),
    WorkspaceFocus,
    WorkspaceDescriptionPart,
  )
where

import Prelude

type WorkspaceDescription = String

type WorkspaceDisplay = String

newtype WorkspaceIndex = WorkspaceIndex String deriving stock (Eq)

instance Show WorkspaceIndex where
  show (WorkspaceIndex index) = index

type WorkspaceFocus = String

type WorkspaceDescriptionPart = String
