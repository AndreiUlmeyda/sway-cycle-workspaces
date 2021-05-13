import Lib (nextWorkspace)
import Mode (Mode (Next, Previous))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types (Workspace (..))

main :: IO ()
main = hspec $ do
  describe "nextWorkspace" $ do
    it "given on input it should return its index" $ do
      let workspaceDesciption = ["output1 1 true"] -- Workspace {output = "output1", workspaceIndex = 1, focused = True}
       in nextWorkspace Previous workspaceDesciption `shouldBe` "1"

-- it "given a two workspaces with the first focused it should return the second index" $ do
--   let workspaceOne = Workspace {output = "output1", workspaceIndex = 1, focused = True}
--   let workspaceTwo = Workspace {output = "output1", workspaceIndex = 2, focused = True}
--    in next [workspaceOne, workspaceTwo] `shouldBe` 2