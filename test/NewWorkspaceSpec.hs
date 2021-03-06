module NewWorkspaceSpec (spec) where

import Data.Either ()
import Mode (Mode (Next, Previous))
import NewWorkspace (changeWorkspace)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Types (WorkspaceIndex (WorkspaceIndex))

spec :: Spec
spec = do
  describe "determining a new workspace number from previous workspaces and command line arguments" $ do
    it "given three workspaces with the second one focused and mode 'Previous' should return the first workspace" $ do
      let workspaceDescription = Right ["output 1 false", "output 2 true", "output 3 false"]
       in changeWorkspace Previous workspaceDescription `shouldBe` Right (WorkspaceIndex "1")

    it "given focus on the first workspace and mode 'Next' should return the second" $ do
      let workspaceDescription = Right ["output 1 true", "output 2 false"]
       in changeWorkspace Next workspaceDescription `shouldBe` Right (WorkspaceIndex "2")

    it "given workspaces on multiple outputs it should only respect the focused output" $ do
      let workspaceDescription = Right ["output1 1 true", "output2 2 false", "output1 3 false"]
       in changeWorkspace Next workspaceDescription `shouldBe` Right (WorkspaceIndex "3")
