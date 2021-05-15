import Data.Either
import Lib (ErrorMessage (ErrorMessage), WorkspaceIndex (WorkspaceIndex), newNextWorkspace, nextWorkspace)
import Mode (Mode (Next, Previous))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types (Workspace (..))

main :: IO ()
main = hspec $ do
  describe "newNextWorkspae" $ do
    it "given no input it should return the empty string" $ do
      newNextWorkspace Previous [] `shouldBe` Left (ErrorMessage "Only zero or one lines/workspaces provided. There is nothing to do.")
    it "given only empty lines should return an error indicating the proper input layout" $ do
      newNextWorkspace Previous ["", "", ""] `shouldBe` Left (ErrorMessage "Each input line must contain 3 items following the pattern 'output workspacename focused' where focused is either 'true' or 'false'")
    it "given a single workspace should return the empty string" $ do
      let workspaceDesciption = ["output 1 true"]
       in newNextWorkspace Previous workspaceDesciption `shouldBe` Left (ErrorMessage "Only zero or one lines/workspaces provided. There is nothing to do.")
    it "given focus on the first workspace and Next-Mode should return the second" $ do
      let workspaceDesciption = ["output 1 true", "output 2 false"]
       in newNextWorkspace Next workspaceDesciption `shouldBe` Right (WorkspaceIndex "2")

-- it "given focus on the last workspace and Next-Mode should return an error indicating there is nothing to do" $ do
--   let workspaceDesciption = ["output 1 false", "output 2 true"]
--    in newNextWorkspace Next workspaceDesciption `shouldBe` Left (ErrorMessage "Only zero or one lines/workspaces provided. There is nothing to do.")