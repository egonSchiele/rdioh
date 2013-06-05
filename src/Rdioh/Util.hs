module Rdioh.Util where
import qualified Data.URLEncoded as UE
import qualified Text.JSON as J
import RdioResult
  
-- convert a list of parameters to a string that can be passed via GET/POST
toParams :: [(String, String)] -> String
toParams = show . UE.importList

(<+>) :: Param p => [(String, String)] -> (String, Maybe p) -> [(String, String)]
arr <+> (_, Nothing) = arr
arr <+> (str, (Just param)) = arr ++ [(str, toParam param)]

class Param a where
    toParam :: a -> String

instance Param Bool where
    toParam True = "true"
    toParam False = "false"

instance Param Int where
    toParam val = show val
