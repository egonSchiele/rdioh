{-# LANGUAGE FlexibleContexts #-}

module Rdioh where
import Rdioh.Auth
import Data.Maybe 
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List.Utils as U
import Control.Monad.Reader
import RdioResult

import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Network.OAuth.Http.CurlHttpClient
-- import Network.OAuth.Http.PercentEncoding
import Rdioh.Util
import Rdioh.Models
import Control.Applicative
import Data.Aeson


-- rdioh :: String -> String -> ReaderT r a -> IO a
runRdioh key secret func = runReaderT func (twoLegToken key secret)

type Rdioh a = ReaderT Token IO a

-- uses the Reader monad to get a token. Then uses that token
-- to make a request to the service url. The returned response
-- is parsed through extractResponse to return the result parsed
-- from JSON to something else.
runRequest :: FromJSON v => [(String, String)] -> Rdioh (Maybe v)
runRequest params = do
    tok <- ask
    let request = srvUrl . B.pack . toParams $ params
    let response = runOAuthM tok $ do
                      request_ <- signRq2 HMACSHA1 Nothing request
                      serviceRequest CurlClient request_
    value <- liftIO $ decode . rspPayload <$> response
    return value
    -- return $ serviceRequest CurlClient response

{-
runRequest params = do
    tok <- ask
    liftM extractResponse $ runOAuthM tok $ signRq2 HMACSHA1 Nothing (srvUrl (B.pack . toParams $ params)) >>= serviceRequest CurlClient

-- extracts just the response from whatever rdio returned
extractResponse = toJSON . B.unpack . rspPayload

-}
-- extractResponse = toJSON . B.unpack . rspPayload



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
-- get keys extras options = runRequest $ [("method", "get"), ("keys", U.join "," keys)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)]) ++ (addMaybe options [("options", jsonify_tuples $ fromJust options)])

-- getObjectFromShortCode short_code extras = runRequest $ [("method", "getObjectFromShortCode"), ("short_code", short_code)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- getObjectFromUrl url extras = runRequest $ [("method", "getObjectFromUrl"), ("url", url)] ++  (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- CATALOG methods

mkExtras :: Show e => [e] -> (String, String)
mkExtras extras = ("extras", U.join "," $ show <$> extras)

getAlbumsByUPC :: String -> [AlbumExtra] -> Rdioh (Maybe [Album])
getAlbumsByUPC upc extras = runRequest $ [("method", "getAlbumsByUPC"), ("upc", upc), mkExtras extras]

getAlbumsForArtist :: String -> Rdioh (Maybe [Album])
getAlbumsForArtist artist = getAlbumsForArtist' artist Nothing [] Nothing Nothing

getAlbumsForArtist' :: String -> Maybe Bool -> [AlbumExtra] -> Maybe Int -> Maybe Int -> Rdioh (Maybe [Album])
getAlbumsForArtist' artist featuring extras start count =
    runRequest $ [("method", "getAlbumsForArtist"), ("artist", artist), mkExtras extras]
                   <+> ("featuring", featuring)
                   <+> ("start", start)
                   <+> ("count", count)

getAlbumsForLabel :: String -> Rdioh (Maybe [Album])
getAlbumsForLabel label = getAlbumsForLabel' label [] Nothing Nothing

getAlbumsForLabel' :: String -> [AlbumExtra] -> Maybe Int -> Maybe Int -> Rdioh (Maybe [Album])
getAlbumsForLabel' label extras start count = 
    runRequest $ [("method", "getAlbumsForLabel"), ("label", label), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

getArtistsForLabel :: String -> Rdioh (Maybe [Artist])
getArtistsForLabel label = getArtistsForLabel' label [] Nothing Nothing

getArtistsForLabel' :: String -> [ArtistExtra] -> Maybe Int -> Maybe Int -> Rdioh (Maybe [Artist])
getArtistsForLabel' label extras start count = 
    runRequest $ [("method", "getArtistsForLabel"), ("label", label), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

-- getTracksByISRC isrc extras = runRequest $ [("method", "getTracksByISRC"), ("isrc", isrc)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- getTracksForArtist artist appears_on start count extras = runRequest $ [("method", "getTracksForArtist"), ("artist", artist)] ++ (addMaybe appears_on [("appears_on", bool_to_s $ fromJust appears_on)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- search query types never_or extras start count = runRequest $ [("method", "search"), ("query", query), ("types", U.join "," (map pretty types))] ++ (addMaybe never_or [("never_or", bool_to_s $ fromJust never_or)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)])
--     where pretty ARTIST_TYPE = "Artist"
--           pretty ALBUM_TYPE = "Album"
--           pretty TRACK_TYPE = "Track"
--           pretty PLAYLIST_TYPE = "Playlist"
--           pretty USER_TYPE = "User"

-- searchSuggestions query extras = runRequest $ [("method", "searchSuggestions"), ("query", query)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- -- COLLECTION methods
-- addToCollection keys = runRequest [("method", "addToCollection"), ("keys", U.join "," keys)]

-- getAlbumsForArtistInCollection artist user extras = runRequest $ [("method", "getAlbumsForArtistInCollection"),("artist", artist)] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- getAlbumsInCollection user start count sort query extras = runRequest $ [("method", "getAlbumsInCollection")] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe sort [("sort", pretty $ fromJust sort)]) ++ (addMaybe query [("query", fromJust query)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
--     where pretty DATE_ADDED_SORT = "dateAdded"
--           pretty PLAY_COUNT_SORT = "playCount"
--           pretty ARTIST_SORT = "artist"
--           pretty NAME_SORT = "name"

-- getArtistsInCollection :: (MonadReader Token m, MonadIO m) => Maybe String -> Maybe Int -> Maybe Int -> Maybe RdioSort -> Maybe String -> Maybe [String] -> m (RdioResult)
-- getArtistsInCollection user start count sort query extras = runRequest $ [("method", "getArtistsInCollection")] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe sort [("sort", pretty $ fromJust sort)]) ++ (addMaybe query [("query", fromJust query)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
--     where pretty DATE_ADDED_SORT = "dateAdded"
--           pretty PLAY_COUNT_SORT = "playCount"
--           pretty ARTIST_SORT = "artist"
--           pretty NAME_SORT = "name"

-- getTracksForAlbumInCollection album user extras = runRequest $ [("method", "getTracksForAlbumInCollection"), ("album", album)] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- getTracksForArtistInCollection artist user extras = runRequest $ [("method", "getTracksForArtistInCollection"), ("artist", artist)] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- getTracksInCollection :: (MonadReader Token m, MonadIO m) => Maybe String -> Maybe Int -> Maybe Int -> Maybe RdioSort -> Maybe String -> Maybe [String] -> m (RdioResult)
-- getTracksInCollection user start count sort query extras = runRequest $ [("method", "getTracksInCollection")] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe sort [("sort", pretty $ fromJust sort)]) ++ (addMaybe query [("query", fromJust query)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
--     where pretty DATE_ADDED_SORT = "dateAdded"
--           pretty PLAY_COUNT_SORT = "playCount"
--           pretty ARTIST_SORT = "artist"
--           pretty NAME_SORT = "name"

-- removeFromCollection keys = runRequest [("method", "removeFromCollection"), ("keys", U.join "," keys)]

-- setAvailableOffline :: (MonadReader Token m, MonadIO m) => [[Char]] -> Bool -> m (RdioResult)
-- setAvailableOffline keys offline = runRequest [("method", "setAvailableOffline"), ("keys", U.join "," keys), ("offline", (bool_to_s offline))]

-- -- PLAYLIST methods
-- addToPlaylist playlist tracks = runRequest [("method", "addToPlaylist"), ("playlist", playlist), ("tracks", U.join "," tracks)]

-- createPlaylist name description tracks extras = runRequest $ [("method", "createPlaylist"), ("name", name), ("description", description), ("tracks", U.join "," tracks)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- deletePlaylist playlist = runRequest [("method", "deletePlaylist"), ("playlist", playlist)]

-- getPlaylists extras = runRequest $ [("method", "getPlaylists")] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- removeFromPlaylist :: (MonadReader Token m, MonadIO m) => String -> Int -> Int -> [[Char]] -> m (RdioResult)
-- removeFromPlaylist playlist index count tracks = runRequest [("method", "removeFromPlaylist"), ("playlist", playlist), ("index", (show index)), ("count", (show count)), ("tracks", U.join "," tracks)]

-- setPlaylistCollaborating :: (MonadReader Token m, MonadIO m) => String -> Bool -> m (RdioResult)
-- setPlaylistCollaborating playlist collaborating = runRequest [("method", "setPlaylistCollaborating"), ("playlist", playlist), ("collaborating", (bool_to_s collaborating))]

-- setPlaylistCollaborationMode :: (MonadReader Token m, MonadIO m) => String -> RdioCollaborationMode -> m (RdioResult)
-- setPlaylistCollaborationMode playlist mode = runRequest [("method", "setPlaylistCollaborationMode"), ("playlist", playlist), ("mode", (pretty mode))]
--     where pretty NO_COLLABORATION = "0"
--           pretty COLLABORATION_WITH_ALL = "1"
--           pretty COLLABORATION_WITH_FOLLOWED = "2"

-- setPlaylistFields playlist name description = runRequest [("method", "setPlaylistFields"), ("playlist", playlist), ("name", name), ("description", description)]

-- setPlaylistOrder playlist tracks = runRequest [("method", "setPlaylistOrder"), ("playlist", playlist), ("tracks", U.join "," tracks)]

-- -- SOCIAL NETWORK methods
-- addFriend user = runRequest [("method", "addFriend"), ("user", user)]

-- currentUser extras = runRequest $ [("method", "currentUser")] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- findUserByEmail email extras = runRequest $ [("method", "findUser"), ("email", email)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- findUserByVanityName vanityName extras = runRequest $ [("method", "findUser"), ("vanityName", vanityName)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- removeFriend user = runRequest [("method", "removeFriend"), ("user", user)]

-- userFollowers :: (MonadReader Token m, MonadIO m) => String -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
-- userFollowers user start count extras = runRequest $ [("method", "userFollowers"), ("user", user)] ++ (addMaybe start [("start", show start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- userFollowing :: (MonadReader Token m, MonadIO m) => String -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
-- userFollowing user start count extras = runRequest $ [("method", "userFollowing"), ("user", user)] ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- -- ACTIVITY AND STATISTICS methods

-- getActivityStream user scope last_id extras = runRequest $ [("method", "getActivityStream"), ("scope", (pretty scope))] ++ (addMaybe last_id [("last_id", fromJust last_id)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
--     where pretty USER_SCOPE = "user"
--           pretty FRIENDS_SCOPE = "friends"
--           pretty EVERYONE_SCOPE = "everyone"

-- getHeavyRotation :: (MonadReader Token m, MonadIO m) => Maybe String -> Maybe RdioObjectType -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
-- getHeavyRotation user object_type friends limit start count extras = runRequest $ [("method", "getHeavyRotation")] ++ (addMaybe user [("user", fromJust user)]) ++ (addMaybe object_type [("object_type", pretty $ fromJust object_type)]) ++ (addMaybe friends [("friends", bool_to_s $ fromJust friends)]) ++ (addMaybe limit [("limit", show $ fromJust limit)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
--     where pretty ARTISTS_OBJECT_TYPE = "artist"
--           pretty ALBUMS_OBJECT_TYPE = "albums"

-- getNewReleases :: (MonadReader Token m, MonadIO m) => Maybe RdioTime -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
-- getNewReleases time start count extras = runRequest $ [("method", "getNewReleases")] ++ (addMaybe time [("time", pretty $ fromJust time)]) ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
--     where pretty THIS_WEEK_TIME = "thisweek"
--           pretty LAST_WEEK_TIME = "lastweek"
--           pretty TWO_WEEKS_TIME = "twoweeks"

-- getTopCharts :: (MonadReader Token m, MonadIO m) => RdioResultType -> Maybe Int -> Maybe Int -> Maybe [String] -> m (RdioResult)
-- getTopCharts result_type start count extras = runRequest $ [("method", "getTopCharts"), ("result_type", (pretty result_type))] ++ (addMaybe start [("start", show $ fromJust start)]) ++ (addMaybe count [("count", show $ fromJust count)]) ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])
--     where pretty ARTIST_RESULT_TYPE = "Artist"
--           pretty ALBUM_RESULT_TYPE = "Album"
--           pretty TRACK_RESULT_TYPE = "Track"
--           pretty PLAYLIST_RESULT_TYPE = "Playlist"

-- -- PLAYBACK methods
-- getPlaybackToken domain = runRequest $ [("method", "getPlaybackToken")] ++ (addMaybe domain [("domain", fromJust domain)])


