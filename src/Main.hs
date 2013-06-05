import Rdioh
import System.IO.Unsafe
import Data.String.Utils
import Rdioh.Models

key = strip . unsafePerformIO . readFile $ "key"
secret = strip . unsafePerformIO . readFile $ "secret"

main = do
    artist <- runRdioh key secret $ searchForArtist "Radiohead"
    case artist of
      Left err -> putStrLn err
      Right radiohead -> do
        albums <- runRdioh key secret $ getAlbumsForArtist (artistKey . head $ radiohead)
        case albums of
          Left err -> putStrLn err
          Right list -> do
            putStrLn "Here are the Radiohead albums on rdio:"
            mapM_ (putStrLn . albumName) list
