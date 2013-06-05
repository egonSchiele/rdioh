import Rdioh
import System.IO.Unsafe
import Data.String.Utils

key = strip . unsafePerformIO . readFile $ "key"
secret = strip . unsafePerformIO . readFile $ "secret"

-- req = get ["a184236"] Nothing Nothing

main = do
    albums <- runRdioh key secret $ getAlbumsForArtist "r91318"
    print albums
