{-# LANGUAGE FlexibleContexts #-}

module Rdio (
twoLegToken,
threeLegToken,
RdioScope(USER_SCOPE, FRIENDS_SCOPE, EVERYONE_SCOPE),
RdioSort(DATE_ADDED_SORT, PLAY_COUNT_SORT, ARTIST_SORT, NAME_SORT),
RdioObjectType(ARTISTS_OBJECT_TYPE, ALBUMS_OBJECT_TYPE),
RdioTime(THIS_WEEK_TIME, LAST_WEEK_TIME, TWO_WEEKS_TIME),
RdioResultType(ARTIST_RESULT_TYPE, ALBUM_RESULT_TYPE, TRACK_RESULT_TYPE, PLAYLIST_RESULT_TYPE),
RdioType(ARTIST_TYPE, ALBUM_TYPE, TRACK_TYPE, PLAYLIST_TYPE, USER_TYPE),
RdioCollaborationMode(NO_COLLABORATION, COLLABORATION_WITH_ALL, COLLABORATION_WITH_FOLLOWED),
get,
getObjectFromShortCode,
getObjectFromUrl,
getAlbumsByUPC,
getAlbumsForArtist,
getTracksByISRC,
getTracksForArtist,
search,
searchSuggestions,
addToCollection,
getAlbumsForArtistInCollection,
getAlbumsInCollection,
getArtistsInCollection,
getTracksForAlbumInCollection,
getTracksForArtistInCollection,
getTracksInCollection,
removeFromCollection,
setAvailableOffline,
addToPlaylist,
createPlaylist,
deletePlaylist,
getPlaylists,
removeFromPlaylist,
setPlaylistCollaborating,
setPlaylistCollaborationMode,
setPlaylistFields,
setPlaylistOrder,
addFriend,
currentUser,
findUserByEmail,
findUserByVanityName,
removeFriend,
userFollowers,
userFollowing,
getActivityStream,
getHeavyRotation,
getNewReleases,
getTopCharts,
getPlaybackToken
) where

import Data.Maybe 
import Network.OAuth.Consumer
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.PercentEncoding
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.URLEncoded as UE
import qualified Data.List.Utils as U
import Control.Monad.Reader
import qualified Text.JSON as J
import qualified Data.List as L

-- functions to convert a JSValue to a RdioResult:

-- this one converts all JSONObjects to nice tuples that can be used to make an RDioDict
conv x = RdioDict $ map (\(str, jsval) -> (str, clean jsval) ) $ J.fromJSObject x

-- helper methods to make all values into RdioResult values so they can be stuffed into
-- an RDioDict
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


(RdioDict xs) ! str = snd . head $ filter (\(key, obj) -> key == str) xs
_ ! _ = error "Only RdioDict types can use '!'"


reqUrl = fromJust . parseURL $ "http://api.rdio.com/oauth/request_token"
accUrl = fromJust . parseURL $ "http://api.rdio.com/oauth/access_token"
authUrl = ("https://www.rdio.com/oauth/authorize?oauth_token="++) . findWithDefault ("oauth_token","ERROR") . oauthParams
srvUrl payload = (fromJust . parseURL $ "http://api.rdio.com/1/") { method     = POST
                                                          , reqPayload = payload
                                                          , reqHeaders = fromList [("content-type", "application/x-www-form-urlencoded")]
                                                          }

app key secret = Application key secret OOB

-- returns a two-legged auth token
twoLegToken key secret   = fromApplication (app key secret)

-- convert a list of parameters to a string that can be passed via GET/POST
toParams :: [(String, String)] -> String
toParams = show . UE.importList

-- given a key and a secret, does three-legged auth and returns an auth token
threeLegToken key secret = runOAuthM (twoLegToken key secret) $ do
    signRq2 HMACSHA1 Nothing reqUrl >>= oauthRequest CurlClient
    cliAskAuthorization authUrl
    signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest CurlClient

-- convert JSON str to parsed
toJSON str = fromResult $ (J.decode str :: J.Result (RdioResult))

-- extracts just the response from whatever rdio returned
extractResponse = toJSON . B.unpack . rspPayload

fromResult (J.Ok x) = x
fromResult (J.Error x) = error x

-- needed b/c haskell capitalizes the first letter otherwise
bool_to_s True = "true"
bool_to_s False = "false"

-- given a list of tuples, return a string that's a JSON object:
jsonify_tuples t = (++"}") . init $ foldl (\acc (k, v) -> acc ++ k ++ ":" ++ v ++ ",") "{" t

-- uses the Reader monad to get a token. Then uses that token
-- to make a request to the service url. The returned response
-- is parsed through extractResponse to return the result parsed
-- from JSON to something else.
runRequest params = do
    tok <- ask    
    liftM extractResponse $ runOAuthM tok $ signRq2 HMACSHA1 Nothing (srvUrl (B.pack . toParams $ params)) >>= serviceRequest CurlClient

-- if the first parameter is a Just, then return the second parameter
-- otherwise return an empty list
addMaybe (Just a) b = b
addMaybe Nothing b = []

-- ADTs for RDIO

data RdioScope = USER_SCOPE | FRIENDS_SCOPE | EVERYONE_SCOPE
data RdioSort = DATE_ADDED_SORT | PLAY_COUNT_SORT | ARTIST_SORT | NAME_SORT
data RdioObjectType = ARTISTS_OBJECT_TYPE | ALBUMS_OBJECT_TYPE
data RdioTime = THIS_WEEK_TIME | LAST_WEEK_TIME | TWO_WEEKS_TIME
data RdioResultType = ARTIST_RESULT_TYPE | ALBUM_RESULT_TYPE | TRACK_RESULT_TYPE | PLAYLIST_RESULT_TYPE
data RdioType = ARTIST_TYPE | ALBUM_TYPE | TRACK_TYPE | PLAYLIST_TYPE | USER_TYPE
data RdioCollaborationMode = NO_COLLABORATION | COLLABORATION_WITH_ALL | COLLABORATION_WITH_FOLLOWED

-- RDIO methods

-- CORE methods
get keys extras options = runRequest $ [("method", "get"), ("keys", U.join "," keys)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)]) ++ (addMaybe options [("options", jsonify_tuples $ fromJust options)])

getObjectFromShortCode short_code extras = runRequest $ [("method", "getObjectFromShortCode"), ("short_code", short_code)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

getObjectFromUrl url extras = runRequest $ [("method", "getObjectFromUrl"), ("url", url)] ++  (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- CATALOG methods
getAlbumsByUPC upc extras = runRequest $ [("method", "getAlbumsByUPC"), ("upc", upc)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

getAlbumsForArtist artist featuring extras start count = runRequest $ [("method", "getAlbumsForArtist"), ("artist", artist)] ++ (addMaybe featuring [("featuring", (bool_to_s $ fromJust featuring))]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)])

getTracksByISRC isrc extras = runRequest $ [("method", "getTracksByISRC"), ("isrc", isrc)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

getTracksForArtist artist appears_on start count extras = runRequest $ [("method", "getTracksForArtist"), ("artist", artist)] ++ (addMaybe appears_on [("appears_on", bool_to_s $ fromJust appears_on)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

search query types never_or extras start count = runRequest $ [("method", "search"), ("query", query), ("types", U.join "," (map pretty types))] ++ (addMaybe never_or [("never_or", bool_to_s $ fromJust never_or)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)])
    where pretty ARTIST_TYPE = "Artist"
          pretty ALBUM_TYPE = "Album"
          pretty TRACK_TYPE = "Track"
          pretty PLAYLIST_TYPE = "Playlist"
          pretty USER_TYPE = "User"

searchSuggestions query extras = runRequest $ [("method", "searchSuggestions"), ("query", query)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- COLLECTION methods
addToCollection keys = runRequest [("method", "addToCollection"), ("keys", U.join "," keys)]

getAlbumsForArtistInCollection artist user extras = runRequest $ [("method", "getAlbumsForArtistInCollection"),("artist", artist)] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

getAlbumsInCollection user start count sort query extras = runRequest $ [("method", "getAlbumsInCollection")] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe sort [("sort", pretty $ fromJust sort)]) ++ (addMaybe query [("query", fromJust query)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
    where pretty DATE_ADDED_SORT = "dateAdded"
          pretty PLAY_COUNT_SORT = "playCount"
          pretty ARTIST_SORT = "artist"
          pretty NAME_SORT = "name"

getArtistsInCollection :: (MonadReader Token m, MonadIO m) => Maybe String -> Maybe Int -> Maybe Int -> Maybe RdioSort -> Maybe String -> Maybe [String] -> m (RdioResult)
getArtistsInCollection user start count sort query extras = runRequest $ [("method", "getArtistsInCollection")] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe sort [("sort", pretty $ fromJust sort)]) ++ (addMaybe query [("query", fromJust query)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
    where pretty DATE_ADDED_SORT = "dateAdded"
          pretty PLAY_COUNT_SORT = "playCount"
          pretty ARTIST_SORT = "artist"
          pretty NAME_SORT = "name"

getTracksForAlbumInCollection album user extras = runRequest $ [("method", "getTracksForAlbumInCollection"), ("album", album)] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

getTracksForArtistInCollection artist user extras = runRequest $ [("method", "getTracksForArtistInCollection"), ("artist", artist)] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

getTracksInCollection :: (MonadReader Token m, MonadIO m) => Maybe String -> Maybe Int -> Maybe Int -> Maybe RdioSort -> Maybe String -> Maybe [String] -> m (RdioResult)
getTracksInCollection user start count sort query extras = runRequest $ [("method", "getTracksInCollection")] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe sort [("sort", pretty $ fromJust sort)]) ++ (addMaybe query [("query", fromJust query)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
    where pretty DATE_ADDED_SORT = "dateAdded"
          pretty PLAY_COUNT_SORT = "playCount"
          pretty ARTIST_SORT = "artist"
          pretty NAME_SORT = "name"

removeFromCollection keys = runRequest [("method", "removeFromCollection"), ("keys", U.join "," keys)]

setAvailableOffline :: (MonadReader Token m, MonadIO m) => [[Char]] -> Bool -> m (RdioResult)
setAvailableOffline keys offline = runRequest [("method", "setAvailableOffline"), ("keys", U.join "," keys), ("offline", (bool_to_s offline))]

-- PLAYLIST methods
addToPlaylist playlist tracks = runRequest [("method", "addToPlaylist"), ("playlist", playlist), ("tracks", U.join "," tracks)]

createPlaylist name description tracks extras = runRequest $ [("method", "createPlaylist"), ("name", name), ("description", description), ("tracks", U.join "," tracks)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

deletePlaylist playlist = runRequest [("method", "deletePlaylist"), ("playlist", playlist)]

getPlaylists extras = runRequest $ [("method", "getPlaylists")] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

removeFromPlaylist :: (MonadReader Token m, MonadIO m) => String -> Int -> Int -> [[Char]] -> m (RdioResult)
removeFromPlaylist playlist index count tracks = runRequest [("method", "removeFromPlaylist"), ("playlist", playlist), ("index", (show index)), ("count", (show count)), ("tracks", U.join "," tracks)]

setPlaylistCollaborating :: (MonadReader Token m, MonadIO m) => String -> Bool -> m (RdioResult)
setPlaylistCollaborating playlist collaborating = runRequest [("method", "setPlaylistCollaborating"), ("playlist", playlist), ("collaborating", (bool_to_s collaborating))]

setPlaylistCollaborationMode :: (MonadReader Token m, MonadIO m) => String -> RdioCollaborationMode -> m (RdioResult)
setPlaylistCollaborationMode playlist mode = runRequest [("method", "setPlaylistCollaborationMode"), ("playlist", playlist), ("mode", (pretty mode))]
    where pretty NO_COLLABORATION = "0"
          pretty COLLABORATION_WITH_ALL = "1"
          pretty COLLABORATION_WITH_FOLLOWED = "2"

setPlaylistFields playlist name description = runRequest [("method", "setPlaylistFields"), ("playlist", playlist), ("name", name), ("description", description)]

setPlaylistOrder playlist tracks = runRequest [("method", "setPlaylistOrder"), ("playlist", playlist), ("tracks", U.join "," tracks)]

-- SOCIAL NETWORK methods
addFriend user = runRequest [("method", "addFriend"), ("user", user)]

currentUser extras = runRequest $ [("method", "currentUser")] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

findUserByEmail email extras = runRequest $ [("method", "findUser"), ("email", email)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

findUserByVanityName vanityName extras = runRequest $ [("method", "findUser"), ("vanityName", vanityName)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

removeFriend user = runRequest [("method", "removeFriend"), ("user", user)]

userFollowers :: (MonadReader Token m, MonadIO m) => String -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
userFollowers user start count extras = runRequest $ [("method", "userFollowers"), ("user", user)] ++ (addMaybe start [("start", show start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

userFollowing :: (MonadReader Token m, MonadIO m) => String -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
userFollowing user start count extras = runRequest $ [("method", "userFollowing"), ("user", user)] ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- ACTIVITY AND STATISTICS methods

-- TODO test this, what happens if you don't pass in a last_id?
getActivityStream user scope last_id extras = runRequest $ [("method", "getActivityStream"), ("scope", (pretty scope))] ++ (addMaybe last_id [("last_id", fromJust last_id)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
    where pretty USER_SCOPE = "user"
          pretty FRIENDS_SCOPE = "friends"
          pretty EVERYONE_SCOPE = "everyone"

getHeavyRotation :: (MonadReader Token m, MonadIO m) => Maybe String -> Maybe RdioObjectType -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
getHeavyRotation user object_type friends limit start count extras = runRequest $ [("method", "getHeavyRotation")] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe object_type [("object_type", pretty $ fromJust object_type)]) ++ (addMaybe friends [("friends", bool_to_s $ fromJust friends)]) ++ (addMaybe limit [("limit", show $ fromJust limit)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
    where pretty ARTISTS_OBJECT_TYPE = "artist"
          pretty ALBUMS_OBJECT_TYPE = "albums"

getNewReleases :: (MonadReader Token m, MonadIO m) => Maybe RdioTime -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
getNewReleases time start count extras = runRequest $ [("method", "getNewReleases")] ++ (addMaybe time [("time", pretty $ fromJust time)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
    where pretty THIS_WEEK_TIME = "thisweek"
          pretty LAST_WEEK_TIME = "lastweek"
          pretty TWO_WEEKS_TIME = "twoweeks"

getTopCharts :: (MonadReader Token m, MonadIO m) => RdioResultType -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
getTopCharts result_type start count extras = runRequest $ [("method", "getTopCharts"), ("result_type", (pretty result_type))] ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
    where pretty ARTIST_RESULT_TYPE = "Artist"
          pretty ALBUM_RESULT_TYPE = "Album"
          pretty TRACK_RESULT_TYPE = "Track"
          pretty PLAYLIST_RESULT_TYPE = "Playlist"

-- PLAYBACK methods
getPlaybackToken domain = runRequest $ [("method", "getPlaybackToken")] ++ (addMaybe domain [("domain", fromJust domain)])


