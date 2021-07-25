module NewWorkspaceSpec (spec) where

import Data.Either ()
import Errors (errorIncorrectCommandLineArgument)
import NewWorkspace (WorkspaceIndex (WorkspaceIndex), changeWorkspace)
import Mode (Mode (Next, Previous))
import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "determining a new workspace number from previous workspaces and command line arguments" $ do
    
    it "given three workspaces with the second one focused and mode 'Previous' should return the first workspace" $ do
      let workspaceDescription = Right ["output 1 false", "output 2 true", "output 3 false"]
       in changeWorkspace (Right Previous) workspaceDescription `shouldBe` Right (WorkspaceIndex "1")
       
    it "given focus on the first workspace and mode 'Next' should return the second" $ do
      let workspaceDescription = Right ["output 1 true", "output 2 false"]
       in changeWorkspace (Right Next) workspaceDescription `shouldBe` Right (WorkspaceIndex "2")
       
    it "given an invalid mode should return an error indicating valid command line arguments" $ do
      let invalidMode = Left errorIncorrectCommandLineArgument
       in changeWorkspace invalidMode (Right []) `shouldBe` invalidMode
       
    it "given workspaces on multiple outputs it should only respect the focused output" $ do
      let workspaceDescription = Right ["output1 1 true", "output2 2 false", "output1 3 false"]
       in changeWorkspace (Right Next) workspaceDescription `shouldBe` Right (WorkspaceIndex "3")