import Data.Either ()
import Errors (ErrorMessage, errorNoNextWorkspace, errorNoPreviousWorkspace, errorNotExactlyOneFocusedWorkspace, errorTooFewInputWorkspaces, errorWrongInputLayout)
import Lib (WorkspaceIndex (WorkspaceIndex), changeWorkspace)
import Mode (Mode (Next, Previous))
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "newNextWorkspae" $ do
    it "given no input it should return an error indicating insufficient input" $ do
      changeWorkspace Previous [] `shouldBe` Left errorTooFewInputWorkspaces
    it "given only empty lines should return an error indicating the proper input layout" $ do
      changeWorkspace Previous ["", "", ""] `shouldBe` Left errorWrongInputLayout
    it "given a single workspace should return an error indicating insufficient input" $ do
      let workspaceDesciption = ["output 1 true"]
       in changeWorkspace Previous workspaceDesciption `shouldBe` Left errorTooFewInputWorkspaces
    it "given focus on the last workspace and mode 'Next' should return an error indicating that there is no next workspace" $ do
      let workspaceDesciption = ["output 1 false", "output 2 true"]
       in changeWorkspace Next workspaceDesciption `shouldBe` Left errorNoNextWorkspace
    it "given focus on the first workspace and mode 'Previous' should return an error indicating that there is no previous workspace" $ do
      let workspaceDesciption = ["output 1 true", "output 2 false"]
       in changeWorkspace Previous workspaceDesciption `shouldBe` Left errorNoPreviousWorkspace
    it "given no focused workspace should return an error indicating a single focused workspace is expected" $ do
      let workspaceDesciption = ["output 1 false", "output 2 false"]
       in changeWorkspace Previous workspaceDesciption `shouldBe` Left errorNotExactlyOneFocusedWorkspace
    it "given two focused workspaces should return an error indicating a single focused workspace is expected" $ do
      let workspaceDesciption = ["output 1 true", "output 2 true"]
       in changeWorkspace Previous workspaceDesciption `shouldBe` Left errorNotExactlyOneFocusedWorkspace
    it "given three workspaces with the second one focused and mode 'Previous' should return the first workspace" $ do
      let workspaceDesciption = ["output 1 false", "output 2 true", "output 3 false"]
       in changeWorkspace Previous workspaceDesciption `shouldBe` Right (WorkspaceIndex "1")
    it "given focus on the first workspace and mode 'Next' should return the second" $ do
      let workspaceDesciption = ["output 1 true", "output 2 false"]
       in changeWorkspace Next workspaceDesciption `shouldBe` Right (WorkspaceIndex "2")
    it "given workspaces on multiple outputs it should only respect the focused output" $ do
      let workspaceDesciption = ["output1 1 true", "output2 2 false", "output1 3 false"]
       in changeWorkspace Next workspaceDesciption `shouldBe` Right (WorkspaceIndex "3")