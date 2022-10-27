import Test.Hspec
import Data.ByteString.Lazy (ByteString)

main :: IO ()
main = hspec $ do
  describe "mainThread" $ do
    it "is request with id was replyed with computed result" $ do
      undefined `shouldBe` (undefined :: ByteString)