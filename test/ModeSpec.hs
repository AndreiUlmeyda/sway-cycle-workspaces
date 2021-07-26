module ModeSpec (spec) where

import Errors (errorIncorrectCommandLineArgument)
import Mode (Mode (Next, Previous), modeFromArgs)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = describe "determining a mode from the command line arguments" $ do
  it "given no input should return an error indicating requirements for valid command line arguments" $ do
    modeFromArgs [] `shouldBe` Left errorIncorrectCommandLineArgument

  it "given an irrelevant argument should return an error indicating valid command line arguments" $ do
    modeFromArgs ["Irrelevant"] `shouldBe` Left errorIncorrectCommandLineArgument

  it "given an argument corresponding to the mode 'next' should return said mode" $ do
    modeFromArgs ["next"] `shouldBe` Right Next

  it "given an argument corresponding to the mode 'previous' should return said mode" $ do
    modeFromArgs ["previous"] `shouldBe` Right Previous

  it "given an argument corresponding to the mode 'next' and an irrelevant one it should just return the mode" $ do
    modeFromArgs ["Irrelevant", "next"] `shouldBe` Right Next
