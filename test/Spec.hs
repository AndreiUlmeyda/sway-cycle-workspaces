import Data.Either ()
import Errors (ErrorMessage, errorNoNextWorkspace, errorTooFewInputWorkspaces, errorWrongInputLayout)
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
    it "given focus on the first workspace and Next-Mode should return the second" $ do
      let workspaceDesciption = ["output 1 true", "output 2 false"]
       in changeWorkspace Next workspaceDesciption `shouldBe` Right (WorkspaceIndex "2")
    it "given focus on the last workspace and Next-Mode should return an error indicating that there is no next workspace" $ do
      let workspaceDesciption = ["output 1 false", "output 2 true"]
       in changeWorkspace Next workspaceDesciption `shouldBe` Left errorNoNextWorkspace