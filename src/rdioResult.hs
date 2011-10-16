module RdioResult where
import qualified Text.JSON as J
import qualified Data.List as L

-- functions to convert a JSValue to a RdioResult:

-- this one converts all JSONObjects to nice tuples that can be used to make an RDioDict
conv x = RdioDict $ map (\(str, jsval) -> (str, clean jsval) ) $ J.fromJSObject x

-- helper methods to make all values into RdioResult values
-- so they can be stuffed into an RDioDict
clean (J.JSObject x) = conv x
clean (J.JSString x) = RdioString $ J.fromJSString x
clean (J.JSBool x) = RdioBool x
clean (J.JSRational _ x) = RdioRational x
clean (J.JSArray xs) = RdioArray $ map clean xs
clean J.JSNull = RdioNull

data RdioResult = RdioDict {rdioDictValue :: [(String, RdioResult)]} | RdioArray {rdioArrayValue :: [RdioResult]} | RdioString {rdioStringValue :: String} | RdioRational {rdioRationalValue :: Rational} | RdioBool {rdioBoolValue :: Bool} | RdioNull

dropLast x l = take (L.genericLength l - x) l

instance Show RdioResult where
    show (RdioDict xs) = "{" ++ (dropLast 2 (foldl (\acc (str, obj) -> acc ++ "\"" ++ str ++ "\"" ++ " : " ++ (show obj) ++ ", ") "" xs)) ++ "}"
    show (RdioArray xs) = "[" ++ (dropLast 2 (foldl (\acc x -> acc ++ (show x) ++ ", ") "" xs)) ++ "]"
    show (RdioString x) = "\"" ++ x ++ "\""
    show (RdioBool x) = show x
    show (RdioRational x) = show x
    show RdioNull = ""


instance J.JSON RdioResult where
    showJSON (RdioDict xs) = J.showJSONs xs
    showJSON (RdioString x) = J.showJSON x
    showJSON (RdioRational x) = J.showJSON (show x)
    showJSON (RdioBool x) = J.showJSON (show x)
    showJSON (RdioArray xs) = J.showJSONs xs
    showJSON RdioNull = J.showJSON ""
    readJSON (J.JSObject obj) = J.Ok $ conv obj

-- helper methods for accessing parts of our RdioResult object
(RdioDict xs) ! str = snd . head $ filter (\(key, obj) -> key == str) xs
(RdioArray xs) .! num = xs !! num

keys (RdioDict xs) = map fst xs
values (RdioDict xs) = map snd xs
