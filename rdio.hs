{-# LANGUAGE FlexibleContexts #-}
module Rdio (twoLegToken, threeLegToken, (!), (.!.), get) where

import Data.Maybe 
import Control.Monad.Trans
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.PercentEncoding
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Monoid
import Debug.Trace
import Data.List.Split
import Data.URLEncoded
import qualified Data.List.Utils as U
import qualified Control.Monad.State as S
import Control.Monad.Reader
import qualified Text.JSON as J
import Control.Monad


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
toParams = show . importList

-- given a key and a secret, does three-legged auth and returns an auth token
threeLegToken key secret = runOAuthM (twoLegToken key secret) $ do
    signRq2 HMACSHA1 Nothing reqUrl >>= oauthRequest CurlClient
    cliAskAuthorization authUrl
    signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest CurlClient

-- convert JSON str to parsed
toJSON str = fromResult $ (J.decode str :: J.Result (J.JSObject J.JSValue))

-- extracts just the response from whatever rdio returned
extractResponse = toJSON . B.unpack . rspPayload

fromResult (J.Ok x) = x
fromResult (J.Error x) = error x

-- needed b/c haskell capitalizes the first letter otherwise
bool_to_s True = "true"
bool_to_s False = "false"

-- given a list of tuples, return a string that's a JSON object:
jsonify_tuples t = (++"}") . init $ foldl (\acc (k, v) -> acc ++ k ++ ":" ++ v ++ ",") "{" t

-- helpers to index into the JSON library's JSObject objects. What a pain.
-- (!) is used when you have a JSObject and you want to access the JSObject that
-- key maps to.
(!) obj key = fromResult $ (J.valFromObj key obj :: J.Result (J.JSObject J.JSValue))
-- (.!.) is used when you have a JSObject and you want to access a JSValue that
-- key maps to.
(.!.) obj key = fromResult $ (J.valFromObj key obj :: J.Result J.JSValue)

-- uses the Reader monad to get a token. Then uses that token
-- to make a request to the service url. The returned response
-- is parsed through extractResponse to return the result parsed
-- from JSON to something else.
runRequest params = do
    tok <- ask    
    liftM extractResponse $ runOAuthM tok $ signRq2 HMACSHA1 Nothing (srvUrl (B.pack . toParams $ params)) >>= serviceRequest CurlClient

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
get keys extras options = runRequest [("method", "get"), ("keys", U.join "," keys), ("extras", U.join "," extras), ("options", jsonify_tuples options)] 

getObjectFromShortCode short_code extras = runRequest [("method", "getObjectFromShortCode"), ("short_code", short_code), ("extras", extras)]

getObjectFromUrl url extras = runRequest [("method", "getObjectFromUrl"), ("url", url), ("extras", extras)]

-- CATALOG methods
getAlbumsByUPC upc extras = runRequest [("method", "getAlbumsByUPC"), ("upc", upc), ("extras", U.join "," extras)]

getAlbumsForArtist :: (MonadReader Token m, MonadIO m) => String -> Bool -> [[Char]] -> Int -> Int -> m (J.JSObject J.JSValue)
getAlbumsForArtist artist featuring extras start count = runRequest [("method", "getAlbumsForArtist"), ("artist", artist), ("featuring", (bool_to_s featuring)), ("extras", U.join "," extras), ("start", (show start)), ("count", (show count))]

getTracksByISRC isrc extras = runRequest [("method", "getTracksByISRC"), ("isrc", isrc), ("extras", U.join "," extras)]

getTracksForArtist :: (MonadReader Token m, MonadIO m) => String -> Bool -> Int -> Int -> [String] -> m (J.JSObject J.JSValue)
getTracksForArtist artist appears_on start count extras = runRequest [("method", "getTracksForArtist"), ("artist", artist), ("appears_on", (bool_to_s appears_on)), ("start", (show start)), ("count", (show count)), ("extras", U.join "," extras)]

search
  :: (MonadReader Token m, MonadIO m) => String -> [RdioType] -> Bool -> [[Char]] -> Int -> Int -> m (J.JSObject J.JSValue)
search query types never_or extras start count = runRequest [("method", "search"), ("query", query), ("types", U.join "," (map pretty types)), ("never_or", (bool_to_s never_or)), ("extras", U.join "," extras), ("start", (show start)), ("count", (show count))]
    where pretty ARTIST_TYPE = "Artist"
          pretty ALBUM_TYPE = "Album"
          pretty TRACK_TYPE = "Track"
          pretty PLAYLIST_TYPE = "Playlist"
          pretty USER_TYPE = "User"

searchSuggestions query extras = runRequest [("method", "searchSuggestions"), ("query", query), ("extras", U.join "," extras)]

-- COLLECTION methods
addToCollection keys = runRequest [("method", "addToCollection"), ("keys", U.join "," keys)]

getAlbumsForArtistInCollection artist user extras = runRequest [("method", "getAlbumsForArtistInCollection"),("artist", artist), ("user", user), ("extras", extras)]

getAlbumsInCollection :: (MonadReader Token m, MonadIO m) => String -> Int -> Int -> RdioSort -> String -> [String] -> m (J.JSObject J.JSValue)
getAlbumsInCollection user start count sort query extras = runRequest [("method", "getAlbumsInCollection"), ("user", user), ("start", (show start)), ("count", (show count)), ("sort", (pretty sort)), ("query", query), ("extras", U.join "," extras)]
    where pretty DATE_ADDED_SORT = "dateAdded"
          pretty PLAY_COUNT_SORT = "playCount"
          pretty ARTIST_SORT = "artist"
          pretty NAME_SORT = "name"

getArtistsInCollection :: (MonadReader Token m, MonadIO m) => String -> Int -> Int -> RdioSort -> String -> [String] -> m (J.JSObject J.JSValue)
getArtistsInCollection user start count sort query extras = runRequest [("method", "getArtistsInCollection"), ("user", user), ("start", (show start)), ("count", (show count)), ("sort", (pretty sort)), ("query", query), ("extras", U.join "," extras)]
    where pretty DATE_ADDED_SORT = "dateAdded"
          pretty PLAY_COUNT_SORT = "playCount"
          pretty ARTIST_SORT = "artist"
          pretty NAME_SORT = "name"

getTracksForAlbumInCollection album user extras = runRequest [("method", "getTracksForAlbumInCollection"), ("album", album), ("user", user), ("extras", U.join "," extras)]


getTracksForArtistInCollection artist user extras = runRequest [("method", "getTracksForArtistInCollection"), ("artist", artist), ("user", user), ("extras", U.join "," extras)]

getTracksInCollection :: (MonadReader Token m, MonadIO m) => String -> Int -> Int -> RdioSort -> String -> [String] -> m (J.JSObject J.JSValue)
getTracksInCollection user start count sort query extras = runRequest [("method", "getTracksInCollection"), ("user", user), ("start", (show start)), ("count", (show count)), ("sort", (pretty sort)), ("query", query), ("extras", U.join "," extras)]
    where pretty DATE_ADDED_SORT = "dateAdded"
          pretty PLAY_COUNT_SORT = "playCount"
          pretty ARTIST_SORT = "artist"
          pretty NAME_SORT = "name"

removeFromCollection keys = runRequest [("method", "removeFromCollection"), ("keys", U.join "," keys)]

setAvailableOffline :: (MonadReader Token m, MonadIO m) => [[Char]] -> Bool -> m (J.JSObject J.JSValue)
setAvailableOffline keys offline = runRequest [("method", "setAvailableOffline"), ("keys", U.join "," keys), ("offline", (bool_to_s offline))]

-- PLAYLIST methods
addToPlaylist playlist tracks = runRequest [("method", "addToPlaylist"), ("playlist", playlist), ("tracks", U.join "," tracks)]

createPlaylist name description tracks extras = runRequest [("method", "createPlaylist"), ("name", name), ("description", description), ("tracks", U.join "," tracks), ("extras", U.join "," extras)]

deletePlaylist playlist = runRequest [("method", "deletePlaylist"), ("playlist", playlist)]

getPlaylists extras = runRequest [("method", "getPlaylists"), ("extras", U.join "," extras)]

removeFromPlaylist :: (MonadReader Token m, MonadIO m) => String -> Int -> Int -> [[Char]] -> m (J.JSObject J.JSValue)
removeFromPlaylist playlist index count tracks = runRequest [("method", "removeFromPlaylist"), ("playlist", playlist), ("index", (show index)), ("count", (show count)), ("tracks", U.join "," tracks)]

setPlaylistCollaborating :: (MonadReader Token m, MonadIO m) => String -> Bool -> m (J.JSObject J.JSValue)
setPlaylistCollaborating playlist collaborating = runRequest [("method", "setPlaylistCollaborating"), ("playlist", playlist), ("collaborating", (bool_to_s collaborating))]

setPlaylistCollaborationMode :: (MonadReader Token m, MonadIO m) => String -> RdioCollaborationMode -> m (J.JSObject J.JSValue)
setPlaylistCollaborationMode playlist mode = runRequest [("method", "setPlaylistCollaborationMode"), ("playlist", playlist), ("mode", (pretty mode))]
    where pretty NO_COLLABORATION = "0"
          pretty COLLABORATION_WITH_ALL = "1"
          pretty COLLABORATION_WITH_FOLLOWED = "2"

setPlaylistFields playlist name description = runRequest [("method", "setPlaylistFields"), ("playlist", playlist), ("name", name), ("description", description)]

setPlaylistOrder playlist tracks = runRequest [("method", "setPlaylistOrder"), ("playlist", playlist), ("tracks", U.join "," tracks)]

-- SOCIAL NETWORK methods
addFriend user = runRequest [("method", "addFriend"), ("user", user)]

currentUser extras = runRequest [("method", "currentUser"), ("extras", U.join "," extras)]

findUserByEmail email = runRequest [("method", "findUser"), ("email", email)]

findUserByVanityName vanityName = runRequest [("method", "findUser"), ("vanityName", vanityName)]

removeFriend user = runRequest [("method", "removeFriend"), ("user", user)]

userFollowers :: (MonadReader Token m, MonadIO m) => String -> Int -> Int -> [String] -> m (J.JSObject J.JSValue)
userFollowers user start count extras = runRequest [("method", "userFollowers"), ("user", user), ("start", (show start)), ("count", (show count)), ("extras", U.join "," extras)]

userFollowing :: (MonadReader Token m, MonadIO m) => String -> Int -> Int -> [String] -> m (J.JSObject J.JSValue)
userFollowing user start count extras = runRequest [("method", "userFollowing"), ("user", user), ("start", (show start)), ("count", (show count)), ("extras", U.join "," extras)]

-- ACTIVITY AND STATISTICS methods

-- TODO test this, what happens if you don't pass in a last_id?
getActivityStream user scope last_id extras = runRequest [("method", "getActivityStream"), ("scope", (pretty scope)), ("last_id", last_id), ("extras", U.join "," extras)]
    where pretty USER_SCOPE = "user"
          pretty FRIENDS_SCOPE = "friends"
          pretty EVERYONE_SCOPE = "everyone"

getHeavyRotation :: (MonadReader Token m, MonadIO m) => String -> RdioObjectType -> Bool -> Int -> Int -> Int -> [String] -> m (J.JSObject J.JSValue)
getHeavyRotation user object_type friends limit start count extras = runRequest [("method", "getHeavyRotation"), ("user", user), ("object_type", (pretty object_type)), ("friends", bool_to_s friends), ("limit", show limit), ("start", (show start)), ("count", (show count)), ("extras", U.join "," extras)]
    where pretty ARTISTS_OBJECT_TYPE = "artist"
          pretty ALBUMS_OBJECT_TYPE = "albums"

getNewReleases :: (MonadReader Token m, MonadIO m) => RdioTime -> Int -> Int -> [String] -> m (J.JSObject J.JSValue)
getNewReleases time start count extras = runRequest [("method", "getNewReleases"), ("time", (pretty time)), ("start", (show start)), ("count", (show count)), ("extras", U.join "," extras)]
    where pretty THIS_WEEK_TIME = "thisweek"
          pretty LAST_WEEK_TIME = "lastweek"
          pretty TWO_WEEKS_TIME = "twoweeks"

getTopCharts :: (MonadReader Token m, MonadIO m) => RdioResultType -> Int -> Int -> [String] -> m (J.JSObject J.JSValue)
getTopCharts result_type start count extras = runRequest [("method", "getTopCharts"), ("result_type", (pretty result_type)), ("start", (show start)), ("count", (show count)), ("extras", U.join "," extras)]
    where pretty ARTIST_RESULT_TYPE = "Artist"
          pretty ALBUM_RESULT_TYPE = "Album"
          pretty TRACK_RESULT_TYPE = "Track"
          pretty PLAYLIST_RESULT_TYPE = "Playlist"

-- PLAYBACK methods
getPlaybackToken domain = runRequest [("method", "getPlaybackToken"), ("domain", domain)]


-- TEST CODE:
key = "[YOUR KEY]"
secret = "[YOUR SECRET]"
tok = twoLegToken key secret

