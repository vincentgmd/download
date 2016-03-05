import Test.Hspec
import Network.Download
import Data.Either (isRight)

main :: IO ()
main = hspec $ do
  describe "openURI" $ do
    it "returns Right data constructor for http" $ do
     doc <- openURI "http://www.google.com"
     doc `shouldSatisfy` isRight

