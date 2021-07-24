import Data.Either ()
import Errors (errorIncorrectCommandLineArgument)
import Lib (WorkspaceIndex (WorkspaceIndex), changeWorkspace)
import Mode (Mode (Next, Previous), modeFromArgs)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
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

  describe "determining a mode from the command line arguments" $ do
    it "given no input should return an error indicating valid command line arguments" $ do
      modeFromArgs [] `shouldBe` Left errorIncorrectCommandLineArgument
    it "given an irrelevant argument should return an error indicating valid command line arguments" $ do
      modeFromArgs ["Irrelevant"] `shouldBe` Left errorIncorrectCommandLineArgument
    it "given an argument corresponding to the mode 'next' should return said mode" $ do
      modeFromArgs ["next"] `shouldBe` Right Next
    it "given an argument corresponding to the mode 'previous' should return said mode" $ do
      modeFromArgs ["previous"] `shouldBe` Right Previous
    it "given an argument corresponding to the mode 'next' and an irrelevant one it should just return the mode" $ do
      modeFromArgs ["Irrelevant", "next"] `shouldBe` Right Next