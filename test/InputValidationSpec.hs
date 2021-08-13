module InputValidationSpec (spec) where

import Errors
  ( errorNoNextWorkspace,
    errorNoPreviousWorkspace,
    errorNotExactlyOneFocusedWorkspace,
    errorTooFewInputWorkspaces,
    errorWrongInputLayout,
  )
import InputValidation (parseInput)
import Mode (Mode (Next, Previous))
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

-- TODO figure out how to test calls to 'error' or stop using it
spec :: Spec
spec = describe "generating, from a mode and an input string, either an error message or a valid worspace description" $ do
  it "given a valid mode but otherwise empty input should generate an error message" $ do
    parseInput Previous "" `shouldBe` Left errorTooFewInputWorkspaces

  it "given a second line which is too short should generate an error indicating the correct layout each line needs to have" $ do
    parseInput Previous "a 1 true\nb" `shouldBe` Left errorWrongInputLayout

  it "given multiple focused outputs should generate an error indicating the ambiguity" $ do
    parseInput Previous "a 1 true\nb 2 true" `shouldBe` Left errorNotExactlyOneFocusedWorkspace

  it "given mode 'Next' and the last output being focused should generate an error indicating that there is no next workspace" $ do
    parseInput Next "a 1 false\nb 2 true" `shouldBe` Left errorNoNextWorkspace

  it "given mode 'Previous' and the first output being focused should generate an error indicating that there is no previous workspace" $ do
    parseInput Previous "a 1 true\nb 2 false" `shouldBe` Left errorNoPreviousWorkspace

  it "given valid inputs it should return a list of workspace descriptions" $ do
    parseInput Next "a 1 true\nb 2 false" `shouldBe` Right ["a 1 true", "b 2 false"]
