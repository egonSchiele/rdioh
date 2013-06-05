import Rdioh
import System.IO.Unsafe
import Data.String.Utils
import Rdioh.Models

key = strip . unsafePerformIO . readFile $ "key"
secret = strip . unsafePerformIO . readFile $ "secret"

main = do
    tracks <- runRdioh3 key secret $ getOfflineTracks
    case tracks of
      Left err -> putStrLn err
      Right t -> print t

