import Rdioh
import System.IO.Unsafe
import Data.String.Utils
import Rdioh.Models

key = strip . unsafePerformIO . readFile $ "key"
secret = strip . unsafePerformIO . readFile $ "secret"

main = do
    albums <- runRdioh key secret $ getAlbumsForArtist "r91318"
    case albums of
      Left err -> putStrLn err
      Right a -> mapM_ (putStrLn . albumName) a
