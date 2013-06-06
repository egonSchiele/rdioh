{-# LANGUAGE FlexibleContexts #-}

module Rdioh where
import Rdioh.Auth
import Data.Either 
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List.Utils as U
import Control.Monad.Reader

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
import qualified Debug.Trace as D

-- | Takes: a key, a secret, a function to run.
runRdio :: String -> String -> Rdio a -> IO a
runRdio key secret func = runReaderT func (twoLegToken key secret)

-- | Same as @runRdio@, but with 3-legged authentication i.e. the user will
-- | have to authorize your app.
runRdioWithAuth :: String -> String -> Rdio a -> IO a
runRdioWithAuth key secret func = do
    tok <- liftIO (threeLegToken key secret)
    runReaderT func tok

-- | The @Rdio@ monad...just a wrapper around a @ReaderT@ monad.
type Rdio a = ReaderT Token IO a

-- | used internally
mkExtras :: Show e => [e] -> (String, String)
mkExtras extras = ("extras", U.join "," $ show <$> extras)

-- | Send a arbitrary request to rdio's api. Return type should
-- | be an instance of @FromJSON@, and you need to specify the type. Example:
--
-- > result <- (runRequest [("method", "getTopCharts"), ("type", "Artist")] :: Rdio (Either String [Artist]))
runRequest :: (Show v, FromJSON v) => [(String, String)] -> Rdio (Either String v)
runRequest params = do
    tok <- ask
    let request = srvUrl . B.pack . toParams $ params
    response <- liftIO $ runOAuthM tok $ do
                      request_ <- signRq2 HMACSHA1 Nothing request
                      serviceRequest CurlClient request_

    -- D.trace (B.unpack . rspPayload $ response) (return ())
    let value = eitherDecode . rspPayload $ response
    return $ rdioResult <$> value

-- RDIO methods

-- CORE methods TODO they all have multiple return types.

-- TODO currently unsupported because it has a variable specification.
-- All you can really do is request an Object back.

-- | Takes: [keys], [extras] (optional)
get :: (Show a, FromJSON a) => [String] -> [String] -> Rdio (Either String a)
get keys extras = runRequest $ [("method", "get"), ("keys", toParam keys), ("extras", toParam extras)]

-- | Takes: short code (everything after the http://rd.io/x/), [extras]
-- (optional)
getObjectFromShortCode :: (Show a, FromJSON a) => String -> [String] -> Rdio (Either String a)
getObjectFromShortCode shortCode extras = runRequest $ [("method", "getObjectFromShortCode"), ("short_code", shortCode), ("extras", toParam extras)]

-- | Takes: url (everything after http://rdio.com/), [extras] (optional)
getObjectFromUrl :: (Show a, FromJSON a) => String -> [String] -> Rdio (Either String a)
getObjectFromUrl url extras = runRequest $ [("method", "getObjectFromUrl"), ("url", url), ("extras", toParam extras)]

-- CATALOG methods

-- | Takes: a UPC code, [extras] (optional)
getAlbumsByUPC :: Int -> [AlbumExtra] -> Rdio (Either String [Album])
getAlbumsByUPC upc extras = runRequest $ [("method", "getAlbumsByUPC"), ("upc", toParam upc), mkExtras extras]

-- | Takes: A key of an artist
getAlbumsForArtist :: String -> Rdio (Either String [Album])
getAlbumsForArtist artist = getAlbumsForArtist' artist Nothing [] Nothing Nothing

getAlbumsForArtist' :: String -> Maybe Bool -> [AlbumExtra] -> Maybe Int -> Maybe Int -> Rdio (Either String [Album])
getAlbumsForArtist' artist featuring extras start count =
    runRequest $ [("method", "getAlbumsForArtist"), ("artist", artist), mkExtras extras]
                   <+> ("featuring", featuring)
                   <+> ("start", start)
                   <+> ("count", count)

-- | Takes: a key of a label
getAlbumsForLabel :: String -> Rdio (Either String [Album])
getAlbumsForLabel label = getAlbumsForLabel' label [] Nothing Nothing

getAlbumsForLabel' :: String -> [AlbumExtra] -> Maybe Int -> Maybe Int -> Rdio (Either String [Album])
getAlbumsForLabel' label extras start count = 
    runRequest $ [("method", "getAlbumsForLabel"), ("label", label), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

-- | Takes: a key of a label
getArtistsForLabel :: String -> Rdio (Either String [Artist])
getArtistsForLabel label = getArtistsForLabel' label [] Nothing Nothing

getArtistsForLabel' :: String -> [ArtistExtra] -> Maybe Int -> Maybe Int -> Rdio (Either String [Artist])
getArtistsForLabel' label extras start count = 
    runRequest $ [("method", "getArtistsForLabel"), ("label", label), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

-- | Takes: an ISRC code, [extras] (optional)
getTracksByISRC :: String -> [TrackExtra] -> Rdio (Either String [Track])
getTracksByISRC isrc extras = runRequest $ [("method", "getTracksByISRC"), ("isrc", isrc), mkExtras extras]

-- | Takes: an artist key
getTracksForArtist :: String -> Rdio (Either String [Track])
getTracksForArtist artist = getTracksForArtist' artist Nothing [] Nothing Nothing

getTracksForArtist' :: String -> Maybe Bool -> [TrackExtra] -> Maybe Int -> Maybe Int -> Rdio (Either String [Track])
getTracksForArtist' artist appears_on extras start count =
    runRequest $ [("method", "getTracksForArtist"), ("artist", artist), mkExtras extras]
                    <+> ("appears_on", appears_on)
                    <+> ("start", start)
                    <+> ("count", count)

-- TODO this should also have an extras field but how to implement that and
-- still be able to return a generic type?
-- | Takes: a query, a type (\"Artist\", \"Album\", \"Track\", \"Playlist\", or
-- \"User\")
-- This method can return any of those types, so you need to specify what
-- you want returned. Example:
--
-- > search "Radiohead" "Artist" :: Rdio (Either String [Artist])
search :: (Show a, FromJSON a) => String -> String -> Rdio (Either String [a])
search query types = search' query types Nothing Nothing Nothing

search' :: (Show a, FromJSON a) => String -> String -> Maybe Bool -> Maybe Int -> Maybe Int -> Rdio (Either String [a])
search' query types neverOr start count = do
    res <- runRequest $ [("method", "search"), ("query", query), ("types", types)]
                   <+> ("never_or", neverOr)
                   <+> ("start", start)
                   <+> ("count", count)

    return (results <$> res)

-- TODO searchSuggestions

-- | Takes: a list of keys of tracks or playlists. *Requires
-- authentication*.
addToCollection :: [String] -> Rdio (Either String Object)
addToCollection keys = runRequest $ [("method", "addToCollection"), ("keys", toParam keys)]

-- | Takes: an artist key. Requires authentication OR use @getAlbumsForArtistInCollection'@ and pass in a user key.
getAlbumsForArtistInCollection :: String -> Rdio (Either String [Album])
getAlbumsForArtistInCollection artist = getAlbumsForArtistInCollection' artist Nothing [] Nothing

getAlbumsForArtistInCollection' :: String -> Maybe String -> [AlbumExtra] -> Maybe String -> Rdio (Either String [Album])
getAlbumsForArtistInCollection' artist user extras sort = 
    runRequest $ [("method", "getAlbumsForArtistInCollection"), ("artist", artist), mkExtras extras]
                  <+> ("user", user)
                  <+> ("sort", sort)

-- | Requires authentication OR use @getAlbumsInCollection'@ and pass in
-- a user key.
getAlbumsInCollection :: Rdio (Either String [Album])
getAlbumsInCollection = getAlbumsInCollection' Nothing Nothing Nothing Nothing Nothing []

getAlbumsInCollection' :: Maybe String -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> [AlbumExtra] -> Rdio (Either String [Album])
getAlbumsInCollection' user start count sort query extras = 
    runRequest $ [("method", "getAlbumsInCollection"), mkExtras extras]
                  <+> ("user", user)
                  <+> ("start", start)
                  <+> ("count", count)
                  <+> ("sort", sort)
                  <+> ("query", query)

-- | Requires authentication OR use @getArtistsInCollection'@ and pass in
-- a user key.
getArtistsInCollection :: Rdio (Either String [Artist])
getArtistsInCollection = getArtistsInCollection' Nothing Nothing Nothing Nothing Nothing []

getArtistsInCollection' :: Maybe String -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> [ArtistExtra] -> Rdio (Either String [Artist])
getArtistsInCollection' user start count sort query extras = 
    runRequest $ [("method", "getArtistsInCollection"), mkExtras extras]
                  <+> ("user", user)
                  <+> ("start", start)
                  <+> ("count", count)
                  <+> ("sort", sort)
                  <+> ("query", query)

-- | Requires authentication.
getOfflineTracks :: Rdio (Either String [Track])
getOfflineTracks = getOfflineTracks' Nothing Nothing []

getOfflineTracks' :: Maybe Int -> Maybe Int -> [TrackExtra] -> Rdio (Either String [Track])
getOfflineTracks' start count extras = 
    runRequest $ [("method", "getOfflineTracks"), mkExtras extras]
                  <+> ("start", start)
                  <+> ("count", count)

-- | Takes: an album key. Requires authentication OR use @getTracksForAlbumInCollection'@ and pass in a user key.
getTracksForAlbumInCollection :: String -> Rdio (Either String [Track])
getTracksForAlbumInCollection album = getTracksForAlbumInCollection' album Nothing []

getTracksForAlbumInCollection' :: String -> Maybe String -> [TrackExtra] -> Rdio (Either String [Track])
getTracksForAlbumInCollection' album user extras =
    runRequest $ [("method", "getTracksForAlbumInCollection"), ("album", album), mkExtras extras]
                  <+> ("user", user)

-- | Takes: an artist key. Requires authentication OR use @getTracksForArtistInCollection'@ and pass in a user key.
getTracksForArtistInCollection :: String -> Rdio (Either String [Track])
getTracksForArtistInCollection artist = getTracksForArtistInCollection' artist Nothing []

getTracksForArtistInCollection' :: String -> Maybe String -> [TrackExtra] -> Rdio (Either String [Track])
getTracksForArtistInCollection' artist user extras =
    runRequest $ [("method", "getTracksForArtistInCollection"), ("artist", artist), mkExtras extras]
                  <+> ("user", user)

-- | Requires authentication OR use @getTracksInCollection'@ and pass in
-- a user key.
getTracksInCollection :: Rdio (Either String [Track])
getTracksInCollection = getTracksInCollection' Nothing Nothing Nothing Nothing Nothing []

getTracksInCollection' :: Maybe String -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> [TrackExtra] -> Rdio (Either String [Track])
getTracksInCollection' user start count sort query extras = 
    runRequest $ [("method", "getTracksInCollection"), mkExtras extras]
                   <+> ("user", user)
                   <+> ("start", start)
                   <+> ("count", count)
                   <+> ("sort", sort)
                   <+> ("query", query)

-- | Takes: a list of track or playlist keys. Requires authentication.
removeFromCollection :: [String] -> Rdio (Either String Bool)
removeFromCollection keys = runRequest $ [("method", "removeFromCollection"), ("keys", toParam keys)]

-- | Takes: a list of track or playlist keys. Requires authentication.
setAvailableOffline :: [String] -> Bool -> Rdio (Either String Object)
setAvailableOffline keys offline = runRequest $ [("method", "setAvailableOffline"), ("keys", toParam keys), ("offline", toParam offline)]

-- | Takes: a playlist key, a list of track keys to add to the playlist,
-- [extras] (optional).
-- Requires authentication.
addToPlaylist :: String -> [String] -> [PlaylistExtra] -> Rdio (Either String Playlist)
addToPlaylist playlist tracks extras = runRequest $ [("method", "addToPlaylist"), ("playlist", playlist), ("tracks", toParam tracks), mkExtras extras]

-- | Takes: a name, a description, a list of track keys to start the
-- playlist with, [extras] (optional). Requires authentication.
createPlaylist :: String -> String -> [String] -> [PlaylistExtra] -> Rdio (Either String Playlist)
createPlaylist name description tracks extras =
    runRequest $ [("method", "createPlaylist"), ("name", name), ("description", description), ("tracks", toParam tracks), mkExtras extras]

-- | Takes: a playlist key. Requires authentication.
deletePlaylist :: String -> Rdio (Either String Bool)
deletePlaylist playlist = runRequest $ [("method", "deletePlaylist"), ("playlist", playlist)]

-- | Requires authentication OR use @getPlaylists'@ and pass in a user key.
getPlaylists :: Rdio (Either String UserPlaylists)
getPlaylists = getPlaylists' Nothing [] Nothing

getPlaylists' :: Maybe String -> [PlaylistExtra] -> Maybe Bool -> Rdio (Either String UserPlaylists)
getPlaylists' user extras orderedList = 
    runRequest $ [("method", "getPlaylists"), mkExtras extras]
                   <+> ("user", user)
                   <+> ("ordered_list", orderedList)

-- | Requires authentication OR use @getUserPlaylists'@ and pass in a user key.
getUserPlaylists :: String -> Rdio (Either String [Playlist])
getUserPlaylists user = getUserPlaylists' user Nothing Nothing Nothing Nothing []

getUserPlaylists' :: String -> Maybe PlaylistType -> Maybe String -> Maybe Int -> Maybe Int -> [PlaylistExtra] -> Rdio (Either String [Playlist])
getUserPlaylists' user kind sort start count extras =
    runRequest $ [("method", "getUserPlaylists"), ("user", user), mkExtras extras]
                   <+> ("kind", kind)
                   <+> ("sort", sort)
                   <+> ("start", start)
                   <+> ("count", count)

-- | Takes:
-- - a playlist key
--
-- - the index of the first item to remove
--
-- - number of tracks to remove
--
-- - the keys of the tracks to remove (redundancy to prevent accidental
-- deletion)
--
-- - [extras] (optional)
--
-- Requires authentication.
removeFromPlaylist :: String -> Int -> Int -> Int -> [PlaylistExtra] -> Rdio (Either String Playlist)
removeFromPlaylist playlist index count tracks extras =
    runRequest $ [("method", "removeFromPlaylist"), ("playlist", playlist), ("index", toParam index), ("count", toParam count), ("tracks", toParam tracks), mkExtras extras]

-- | Takes: a playlist key, a boolean (true == collaborating, false == not
-- collaborating). Requires authentication.
setPlaylistCollaborating :: String -> Bool -> Rdio (Either String Bool)
setPlaylistCollaborating playlist collaborating =
    runRequest $ [("method", "setPlaylistCollaborating"), ("playlist", playlist), ("collaborating", toParam collaborating)]

-- | Takes: a playlist key, a collaboration mode. Requires authentication.
setPlaylistCollaborationMode :: String -> CollaborationMode -> Rdio (Either String Bool)
setPlaylistCollaborationMode playlist mode =
    runRequest $ [("method", "setPlaylistCollaborationMode"), ("playlist", playlist), ("mode", toParam mode)]

-- | Takes: a playlist key, a name, a description. Requires authentication.
setPlaylistFields :: String -> String -> String -> Rdio (Either String Bool)
setPlaylistFields playlist name description =
    runRequest $ [("method", "setPlaylistFields"), ("playlist", playlist), ("name", name), ("description", description)]

-- | Takes: a playlist key, a list of track keys, [extras] (optional).
-- Requires authentication.
setPlaylistOrder :: String -> [String] -> [PlaylistExtra] -> Rdio (Either String Playlist)
setPlaylistOrder playlist tracks extras =
    runRequest $ [("method", "setPlaylistOrder"), ("playlist", playlist), ("tracks", toParam tracks), mkExtras extras]

-- | Takes: a user key. Requires authentication.
addFriend :: String -> Rdio (Either String Bool)
addFriend user = runRequest $ [("method", "addFriend"), ("user", user)]

-- | Requires authentication.
currentUser :: [UserExtra] -> Rdio (Either String User)
currentUser extras = runRequest $ [("method", "currentUser"), mkExtras extras]

-- | Takes: an email address, [extras] (optional).
findUserByEmail :: String -> [UserExtra] -> Rdio (Either String User)
findUserByEmail email extras = runRequest $ [("method", "findUser"), ("email", email), mkExtras extras]

-- | Takes: user name, [extras] (optional).
findUserByName :: String -> [UserExtra] -> Rdio (Either String User)
findUserByName vanityName extras = runRequest $ [("method", "findUser"), ("vanityName", vanityName), mkExtras extras]

-- | Takes: a user key. Requires authentication.
removeFriend :: String -> Rdio (Either String Bool)
removeFriend user = runRequest $ [("method", "removeFriend"), ("user", user)]

-- | Takes: a user key.
userFollowers :: String -> Rdio (Either String [User])
userFollowers user = userFollowers' user Nothing Nothing [] Nothing

userFollowers' :: String -> Maybe Int -> Maybe Int -> [UserExtra] -> Maybe String -> Rdio (Either String [User])
userFollowers' user start count extras inCommon =
    runRequest $ [("method", "userFollowers"), ("user", user), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)
                   <+> ("inCommon", inCommon)

-- | Takes: a user key.
userFollowing :: String -> Rdio (Either String [User])
userFollowing user = userFollowing' user Nothing Nothing [] Nothing

userFollowing' :: String -> Maybe Int -> Maybe Int -> [UserExtra] -> Maybe String -> Rdio (Either String [User])
userFollowing' user start count extras inCommon =
    runRequest $ [("method", "userFollowing"), ("user", user), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)
                   <+> ("inCommon", inCommon)


-- TODO extras not implemented here because I don't know what it means in
-- this context. Also, the docs say there will be additional data depending
-- on the update_type. Without specifying crap. So this is incomplete for
-- now.

-- | Takes: a user key, a scope.
getActivityStream :: String -> Scope -> Rdio (Either String Activity)
getActivityStream user scope = getActivityStream' user scope Nothing Nothing

getActivityStream' :: String -> Scope -> Maybe Int -> Maybe Int -> Rdio (Either String Activity)
getActivityStream' user scope lastId count = 
    runRequest $ [("method", "getActivityStream"), ("user", user), ("scope", toParam scope)]
                   <+> ("last_id", lastId)
                   <+> ("count", count)

getHeavyRotationArtists :: Rdio (Either String [Artist])
getHeavyRotationArtists = getHeavyRotationArtists' Nothing Nothing Nothing Nothing Nothing []

getHeavyRotationArtists' :: Maybe String -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> [ArtistExtra] -> Rdio (Either String [Artist])
getHeavyRotationArtists' user friends limit start count extras =
    runRequest $ [("method", "getHeavyRotation"), ("type", "artists"), mkExtras extras]
                   <+> ("user", user)
                   <+> ("friends", friends)
                   <+> ("limit", limit)
                   <+> ("start", start)
                   <+> ("count", count)

getHeavyRotationAlbums :: Rdio (Either String [Album])
getHeavyRotationAlbums = getHeavyRotationAlbums' Nothing Nothing Nothing Nothing Nothing []

getHeavyRotationAlbums' :: Maybe String -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> [AlbumExtra] -> Rdio (Either String [Album])
getHeavyRotationAlbums' user friends limit start count extras =
    runRequest $ [("method", "getHeavyRotation"), ("type", "artists"), mkExtras extras]
                   <+> ("user", user)
                   <+> ("friends", friends)
                   <+> ("limit", limit)
                   <+> ("start", start)
                   <+> ("count", count)

getNewReleases :: Rdio (Either String [Album])
getNewReleases = getNewReleases' Nothing Nothing Nothing []

getNewReleases' :: Maybe Timeframe -> Maybe Int -> Maybe Int -> [AlbumExtra] -> Rdio (Either String [Album])
getNewReleases' time start count extras =
    runRequest $ [("method", "getNewReleases"), mkExtras extras]
                   <+> ("time", time)
                   <+> ("start", start)
                   <+> ("count", count)

getTopChartArtists :: Rdio (Either String [Artist])
getTopChartArtists = getTopChartArtists' Nothing Nothing []

getTopChartArtists' :: Maybe Int -> Maybe Int -> [ArtistExtra] -> Rdio (Either String [Artist])
getTopChartArtists' start count extras =
    runRequest $ [("method", "getTopCharts"), ("type", "Artist"), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

getTopChartAlbums :: Rdio (Either String [Album])
getTopChartAlbums = getTopChartAlbums' Nothing Nothing []

getTopChartAlbums' :: Maybe Int -> Maybe Int -> [AlbumExtra] -> Rdio (Either String [Album])
getTopChartAlbums' start count extras =
    runRequest $ [("method", "getTopCharts"), ("type", "Album"), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)


getTopChartTracks :: Rdio (Either String [Track])
getTopChartTracks = getTopChartTracks' Nothing Nothing []

getTopChartTracks' :: Maybe Int -> Maybe Int -> [TrackExtra] -> Rdio (Either String [Track])
getTopChartTracks' start count extras =
    runRequest $ [("method", "getTopCharts"), ("type", "Track"), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)


getTopChartPlaylists :: Rdio (Either String [Playlist])
getTopChartPlaylists = getTopChartPlaylists' Nothing Nothing []

getTopChartPlaylists' :: Maybe Int -> Maybe Int -> [PlaylistExtra] -> Rdio (Either String [Playlist])
getTopChartPlaylists' start count extras =
    runRequest $ [("method", "getTopCharts"), ("type", "Playlist"), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

-- | Takes: the domain that the playback SWF will be embedded in
-- (optional).
getPlaybackToken :: Maybe String -> Rdio (Either String String)
getPlaybackToken domain = runRequest $ [("method", "getPlaybackToken")] <+> ("domain", domain)
