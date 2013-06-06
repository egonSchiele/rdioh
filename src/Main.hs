import Rdioh
import System.IO.Unsafe
import Data.String.Utils
import Rdioh.Models
import Data.String.Utils

key = strip . unsafePerformIO . readFile $ "key"
secret = strip . unsafePerformIO . readFile $ "secret"

main = do
    tracks <- runRdio key secret $ getAlbumsForArtist "r123"
    case tracks of
      Left err -> putStrLn err
      Right t -> print t

