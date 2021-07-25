module InputValidationSpec ( spec ) where

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe )
import InputValidation ( parseInput )
import Mode ( Mode ( Previous, Next ) )
import Errors
  ( errorTooFewInputWorkspaces
  , errorWrongInputLayout
  , errorNotExactlyOneFocusedWorkspace
  , errorNoNextWorkspace
  , errorNoPreviousWorkspace )

spec :: Spec
spec = describe "generating, from a mode and an input string, either an error message or a valid worspace description" $ do

  it "given a valid mode but otherwise empty input should generate an error message" $ do
    parseInput (Right Previous) "" `shouldBe` Left errorTooFewInputWorkspaces

  it "given a second line which is too short should generate an error indicating the correct layout each line needs to have" $ do
    parseInput (Right Previous) "a 1 true\nb" `shouldBe` Left errorWrongInputLayout

  it "given multiple focused outputs should generate an error indicating the ambiguity" $ do
    parseInput (Right Previous) "a 1 true\nb 2 true" `shouldBe` Left errorNotExactlyOneFocusedWorkspace

  it "given mode 'Next' and the last output being focused should generate an error indicating that there is no next workspace" $ do
    parseInput (Right Next) "a 1 false\nb 2 true" `shouldBe` Left errorNoNextWorkspace

  it "given mode 'Previous' and the first output being focused should generate an error indicating that there is no previous workspace" $ do
    parseInput (Right Previous) "a 1 true\nb 2 false" `shouldBe` Left errorNoPreviousWorkspace

  it "given valid inputs it should return a list of workspace descriptions" $ do
    parseInput (Right Next) "a 1 true\nb 2 false" `shouldBe` Right ["a 1 true","b 2 false"]
