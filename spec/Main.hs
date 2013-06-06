import Test.Hspec
import Rdioh
import System.IO.Unsafe
import Data.String.Utils
import Data.Aeson

key = strip . unsafePerformIO . readFile $ "../key"
secret = strip . unsafePerformIO . readFile $ "../secret"

testMethod :: FromJSON a => Rdio (Either String a) -> IO Bool
testMethod meth = do
    result <- runRdio key secret meth
    case result of
      Left err -> fail err
      Right _ -> return True

main = hspec $ do
  describe "test methods" $ do
    it "getAlbumsByUPC" $ do
      unsafePerformIO $ testMethod $ getAlbumsByUPC "654979031628" []
