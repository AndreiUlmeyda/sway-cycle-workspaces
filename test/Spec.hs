import Lib (next)
import Test.Hspec
import Types (Workspace (..))

main :: IO ()
main = hspec $ do
  describe "next" $ do
    it "given a single workspace it should return its index" $ do
      let workspaceOne = Workspace {output = "output1", workspaceIndex = 1, focused = True}
       in next [workspaceOne] `shouldBe` 1
    it "given a two workspaces with the first focused it should return the second index" $ do
      let workspaceOne = Workspace {output = "output1", workspaceIndex = 1, focused = True}
      let workspaceTwo = Workspace {output = "output1", workspaceIndex = 2, focused = True}
       in next [workspaceOne, workspaceTwo] `shouldBe` 2