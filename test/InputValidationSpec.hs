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
    shouldBe, shouldThrow, errorCall,  )
import Control.Exception.Base (evaluate)

spec :: Spec
spec = describe "generating, from a mode and an input string, either an error message or a valid worspace description" $ do
  it "given a valid mode but otherwise empty input should generate an error message" $ do
    evaluate (parseInput Previous "") `shouldThrow` errorCall (show errorTooFewInputWorkspaces)

  it "given a second line which is too short should generate an error indicating the correct layout each line needs to have" $ do
    evaluate (parseInput Previous "a 1 true\nb") `shouldThrow` errorCall (show errorWrongInputLayout)

  it "given multiple focused outputs should generate an error indicating the ambiguity" $ do
    evaluate (parseInput Previous "a 1 true\nb 2 true") `shouldThrow` errorCall (show errorNotExactlyOneFocusedWorkspace)

  it "given mode 'Next' and the last output being focused should generate an error indicating that there is no next workspace" $ do
    evaluate (parseInput Next "a 1 false\nb 2 true") `shouldThrow` errorCall (show errorNoNextWorkspace)

  it "given mode 'Previous' and the first output being focused should generate an error indicating that there is no previous workspace" $ do
    evaluate (parseInput Previous "a 1 true\nb 2 false") `shouldThrow` errorCall (show errorNoPreviousWorkspace)

  it "given valid inputs it should return a list of workspace descriptions" $ do
    parseInput Next "a 1 true\nb 2 false" `shouldBe` ["a 1 true", "b 2 false"]
