{-# LANGUAGE FlexibleContexts #-}

module Rdioh where
import Rdioh.Auth
import Data.Either 
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
import Data.Map
import qualified Debug.Trace as D

runRdioh :: String -> String -> Rdioh a -> IO a
runRdioh key secret func = runReaderT func (twoLegToken key secret)

runRdioh3 :: String -> String -> Rdioh a -> IO a
runRdioh3 key secret func = do
    tok <- liftIO (threeLegToken key secret)
    runReaderT func tok

type Rdioh a = ReaderT Token IO a

-- uses the Reader monad to get a token. Then uses that token
-- to make a request to the service url.
runRequest :: (Show v, FromJSON v) => [(String, String)] -> Rdioh (Either String v)
runRequest params = do
    tok <- ask
    let request = srvUrl . B.pack . toParams $ params
    response <- liftIO $ runOAuthM tok $ do
                      request_ <- signRq2 HMACSHA1 Nothing request
                      serviceRequest CurlClient request_

    -- D.trace (show . rspPayload $ response) (return ())
    let value = eitherDecode . rspPayload $ response
    return $ rdioResult <$> value

-- RDIO methods

-- CORE methods TODO they all have multiple return types.
-- get keys extras options = runRequest $ [("method", "get"), ("keys", U.join "," keys)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)]) ++ (addMaybe options [("options", jsonify_tuples $ fromJust options)])

-- getObjectFromShortCode short_code extras = runRequest $ [("method", "getObjectFromShortCode"), ("short_code", short_code)] ++ (addMaybe extras [("extras", U.join "," $ fromJust extras)])

-- getObjectFromUrl url extras = runRequest $ [("method", "getObjectFromUrl"), ("url", url)] ++  (addEither extras [("extras", U.join "," $ fromJust extras)])

-- CATALOG methods

mkExtras :: Show e => [e] -> (String, String)
mkExtras extras = ("extras", U.join "," $ show <$> extras)

getAlbumsByUPC :: String -> [AlbumExtra] -> Rdioh (Either String [Album])
getAlbumsByUPC upc extras = runRequest $ [("method", "getAlbumsByUPC"), ("upc", upc), mkExtras extras]

getAlbumsForArtist :: String -> Rdioh (Either String [Album])
getAlbumsForArtist artist = getAlbumsForArtist' artist Nothing [] Nothing Nothing

getAlbumsForArtist' :: String -> Maybe Bool -> [AlbumExtra] -> Maybe Int -> Maybe Int -> Rdioh (Either String [Album])
getAlbumsForArtist' artist featuring extras start count =
    runRequest $ [("method", "getAlbumsForArtist"), ("artist", artist), mkExtras extras]
                   <+> ("featuring", featuring)
                   <+> ("start", start)
                   <+> ("count", count)

getAlbumsForLabel :: String -> Rdioh (Either String [Album])
getAlbumsForLabel label = getAlbumsForLabel' label [] Nothing Nothing

getAlbumsForLabel' :: String -> [AlbumExtra] -> Maybe Int -> Maybe Int -> Rdioh (Either String [Album])
getAlbumsForLabel' label extras start count = 
    runRequest $ [("method", "getAlbumsForLabel"), ("label", label), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

getArtistsForLabel :: String -> Rdioh (Either String [Artist])
getArtistsForLabel label = getArtistsForLabel' label [] Nothing Nothing

getArtistsForLabel' :: String -> [ArtistExtra] -> Maybe Int -> Maybe Int -> Rdioh (Either String [Artist])
getArtistsForLabel' label extras start count = 
    runRequest $ [("method", "getArtistsForLabel"), ("label", label), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

getTracksByISRC :: String -> [TrackExtra] -> Rdioh (Either String [Track])
getTracksByISRC isrc extras = runRequest $ [("method", "getTracksByISRC"), ("isrc", isrc), mkExtras extras]

getTracksForArtist :: String -> Rdioh (Either String [Track])
getTracksForArtist artist = getTracksForArtist' artist Nothing [] Nothing Nothing

getTracksForArtist' :: String -> Maybe Bool -> [TrackExtra] -> Maybe Int -> Maybe Int -> Rdioh (Either String [Track])
getTracksForArtist' artist appears_on extras start count =
    runRequest $ [("method", "getTracksForArtist"), ("artist", artist), mkExtras extras]
                    <+> ("appears_on", appears_on)
                    <+> ("start", start)
                    <+> ("count", count)

-- TODO implement search for everything else...maybe generate this
-- programmatically?
searchForArtist :: String -> Rdioh (Either String [Artist])
searchForArtist query = searchForArtist' query Nothing [] Nothing Nothing

searchForArtist' :: String -> Maybe Bool -> [ArtistExtra] -> Maybe Int -> Maybe Int -> Rdioh (Either String [Artist])
searchForArtist' query never_or extras start count = do
    res <- runRequest $ [("method", "search"), ("query", query), ("types", "Artist"), mkExtras extras]
                   <+> ("never_or", never_or)
                   <+> ("start", start)
                   <+> ("count", count)

    return (results <$> res)

-- TODO searchSuggestions

getOfflineTracks :: Rdioh (Either String [Track])
getOfflineTracks = getOfflineTracks' Nothing Nothing []

getOfflineTracks' :: Maybe Int -> Maybe Int -> [TrackExtra] -> Rdioh (Either String [Track])
getOfflineTracks' start count extras = 
    runRequest $ [("method", "getOfflineTracks"), mkExtras extras]
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


