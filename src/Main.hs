import Rdioh
import System.IO.Unsafe
import Rdioh.Models
import Data.String.Utils
import Control.Monad.IO.Class

key = strip . unsafePerformIO . readFile $ "key"
secret = strip . unsafePerformIO . readFile $ "secret"

main = do
    runRdio key secret $ do
      res1 <- search "Radiohead" "Artist" :: Rdio (Either String [Artist])
      case res1 of
        Left err -> liftIO $ putStrLn err
        Right (radiohead:_) -> do
          res2 <- getAlbumsForArtist (artistKey radiohead)
          case res2 of
            Left err -> liftIO $ putStrLn err
            Right albums -> liftIO $ mapM_ (putStrLn . albumName) albums
