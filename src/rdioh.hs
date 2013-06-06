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

runRdio :: String -> String -> Rdio a -> IO a
runRdio key secret func = runReaderT func (twoLegToken key secret)

runRdioWithAuth :: String -> String -> Rdio a -> IO a
runRdioWithAuth key secret func = do
    tok <- liftIO (threeLegToken key secret)
    runReaderT func tok

type Rdio a = ReaderT Token IO a

-- uses the Reader monad to get a token. Then uses that token
-- to make a request to the service url.
runRequest :: (Show v, FromJSON v) => [(String, String)] -> Rdio (Either String v)
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

-- TODO this also has extras and options fields.
-- extras: how to do this and still maintain a generic return type?
-- options: what is this used for? No specification on api page...
get :: (Show a, FromJSON a) => [String] -> Rdio (Either String a)
get keys = runRequest $ [("method", "get"), ("keys", toParam keys )]

getObjectFromShortCode :: (Show a, FromJSON a) => String -> Rdio (Either String a)
getObjectFromShortCode shortCode = runRequest $ [("method", "getObjectFromShortCode"), ("short_code", shortCode)]

getObjectFromUrl :: (Show a, FromJSON a) => String -> Rdio (Either String a)
getObjectFromUrl url = runRequest $ [("method", "getObjectFromUrl"), ("url", url)]

-- CATALOG methods

mkExtras :: Show e => [e] -> (String, String)
mkExtras extras = ("extras", U.join "," $ show <$> extras)

getAlbumsByUPC :: Int -> [AlbumExtra] -> Rdio (Either String [Album])
getAlbumsByUPC upc extras = runRequest $ [("method", "getAlbumsByUPC"), ("upc", toParam upc), mkExtras extras]

getAlbumsForArtist :: String -> Rdio (Either String [Album])
getAlbumsForArtist artist = getAlbumsForArtist' artist Nothing [] Nothing Nothing

getAlbumsForArtist' :: String -> Maybe Bool -> [AlbumExtra] -> Maybe Int -> Maybe Int -> Rdio (Either String [Album])
getAlbumsForArtist' artist featuring extras start count =
    runRequest $ [("method", "getAlbumsForArtist"), ("artist", artist), mkExtras extras]
                   <+> ("featuring", featuring)
                   <+> ("start", start)
                   <+> ("count", count)

getAlbumsForLabel :: String -> Rdio (Either String [Album])
getAlbumsForLabel label = getAlbumsForLabel' label [] Nothing Nothing

getAlbumsForLabel' :: String -> [AlbumExtra] -> Maybe Int -> Maybe Int -> Rdio (Either String [Album])
getAlbumsForLabel' label extras start count = 
    runRequest $ [("method", "getAlbumsForLabel"), ("label", label), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

getArtistsForLabel :: String -> Rdio (Either String [Artist])
getArtistsForLabel label = getArtistsForLabel' label [] Nothing Nothing

getArtistsForLabel' :: String -> [ArtistExtra] -> Maybe Int -> Maybe Int -> Rdio (Either String [Artist])
getArtistsForLabel' label extras start count = 
    runRequest $ [("method", "getArtistsForLabel"), ("label", label), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)

getTracksByISRC :: String -> [TrackExtra] -> Rdio (Either String [Track])
getTracksByISRC isrc extras = runRequest $ [("method", "getTracksByISRC"), ("isrc", isrc), mkExtras extras]

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

addToCollection :: [String] -> Rdio (Either String Object)
addToCollection keys = runRequest $ [("method", "addToCollection"), ("keys", toParam keys)]

getAlbumsForArtistInCollection :: String -> Rdio (Either String [Album])
getAlbumsForArtistInCollection artist = getAlbumsForArtistInCollection' artist Nothing [] Nothing

getAlbumsForArtistInCollection' :: String -> Maybe String -> [AlbumExtra] -> Maybe String -> Rdio (Either String [Album])
getAlbumsForArtistInCollection' artist user extras sort = 
    runRequest $ [("method", "getAlbumsForArtistInCollection"), ("artist", artist), mkExtras extras]
                  <+> ("user", user)
                  <+> ("sort", sort)

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

getOfflineTracks :: Rdio (Either String [Track])
getOfflineTracks = getOfflineTracks' Nothing Nothing []

getOfflineTracks' :: Maybe Int -> Maybe Int -> [TrackExtra] -> Rdio (Either String [Track])
getOfflineTracks' start count extras = 
    runRequest $ [("method", "getOfflineTracks"), mkExtras extras]
                  <+> ("start", start)
                  <+> ("count", count)

getTracksForAlbumInCollection :: String -> Rdio (Either String [Track])
getTracksForAlbumInCollection album = getTracksForAlbumInCollection' album Nothing []

getTracksForAlbumInCollection' :: String -> Maybe String -> [TrackExtra] -> Rdio (Either String [Track])
getTracksForAlbumInCollection' album user extras =
    runRequest $ [("method", "getTracksForAlbumInCollection"), ("album", album), mkExtras extras]
                  <+> ("user", user)

getTracksForArtistInCollection :: String -> Rdio (Either String [Track])
getTracksForArtistInCollection artist = getTracksForArtistInCollection' artist Nothing []

getTracksForArtistInCollection' :: String -> Maybe String -> [TrackExtra] -> Rdio (Either String [Track])
getTracksForArtistInCollection' artist user extras =
    runRequest $ [("method", "getTracksForArtistInCollection"), ("album", artist), mkExtras extras]
                  <+> ("user", user)

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

removeFromCollection :: [String] -> Rdio (Either String Bool)
removeFromCollection keys = runRequest $ [("method", "removeFromCollection"), ("keys", toParam keys)]

setAvailableOffline :: [String] -> Bool -> Rdio (Either String Object)
setAvailableOffline keys offline = runRequest $ [("method", "setAvailableOffline"), ("keys", toParam keys), ("offline", toParam offline)]

addToPlaylist :: String -> [String] -> [PlaylistExtra] -> Rdio (Either String Playlist)
addToPlaylist playlist tracks extras = runRequest $ [("method", "addToPlaylist"), ("playlist", playlist), ("tracks", toParam tracks), mkExtras extras]

createPlaylist :: String -> String -> [String] -> [PlaylistExtra] -> Rdio (Either String Playlist)
createPlaylist name description tracks extras =
    runRequest $ [("method", "createPlaylist"), ("name", name), ("description", description), ("tracks", toParam tracks), mkExtras extras]

deletePlaylist :: String -> Rdio (Either String Bool)
deletePlaylist playlist = runRequest $ [("method", "deletePlaylist"), ("playlist", playlist)]

getPlaylists :: Rdio (Either String UserPlaylists)
getPlaylists = getPlaylists' Nothing [] Nothing

getPlaylists' :: Maybe String -> [PlaylistExtra] -> Maybe Bool -> Rdio (Either String UserPlaylists)
getPlaylists' user extras orderedList = 
    runRequest $ [("method", "getPlaylists"), mkExtras extras]
                   <+> ("user", user)
                   <+> ("ordered_list", orderedList)

getUserPlaylists :: String -> Rdio (Either String [Playlist])
getUserPlaylists user = getUserPlaylists' user Nothing Nothing Nothing Nothing []

getUserPlaylists' :: String -> Maybe PlaylistType -> Maybe String -> Maybe Int -> Maybe Int -> [PlaylistExtra] -> Rdio (Either String [Playlist])
getUserPlaylists' user kind sort start count extras =
    runRequest $ [("method", "getUserPlaylists"), ("user", user), mkExtras extras]
                   <+> ("kind", kind)
                   <+> ("sort", sort)
                   <+> ("start", start)
                   <+> ("count", count)

removeFromPlaylist :: String -> Int -> Int -> Int -> [PlaylistExtra] -> Rdio (Either String Playlist)
removeFromPlaylist playlist index count tracks extras =
    runRequest $ [("method", "removeFromPlaylist"), ("playlist", playlist), ("index", toParam index), ("count", toParam count), ("tracks", toParam tracks), mkExtras extras]

setPlaylistCollaborating :: String -> Bool -> Rdio (Either String Bool)
setPlaylistCollaborating playlist collaborating =
    runRequest $ [("method", "setPlaylistCollaborating"), ("playlist", playlist), ("collaborating", toParam collaborating)]

setPlaylistCollaborationMode :: String -> CollaborationMode -> Rdio (Either String Bool)
setPlaylistCollaborationMode playlist mode =
    runRequest $ [("method", "setPlaylistCollaborationMode"), ("playlist", playlist), ("mode", toParam mode)]

setPlaylistFields :: String -> String -> String -> Rdio (Either String Bool)
setPlaylistFields playlist name description =
    runRequest $ [("method", "setPlaylistFields"), ("playlist", playlist), ("name", name), ("description", description)]

setPlaylistOrder :: String -> [String] -> [PlaylistExtra] -> Rdio (Either String Playlist)
setPlaylistOrder playlist tracks extras =
    runRequest $ [("method", "setPlaylistOrder"), ("playlist", playlist), ("tracks", toParam tracks), mkExtras extras]

addFriend :: String -> Rdio (Either String Bool)
addFriend user = runRequest $ [("method", "addFriend"), ("user", user)]

currentUser :: [UserExtra] -> Rdio (Either String User)
currentUser extras = runRequest $ [("method", "currentUser"), mkExtras extras]

findUserByEmail :: String -> [UserExtra] -> Rdio (Either String User)
findUserByEmail email extras = runRequest $ [("method", "findUser"), ("email", email), mkExtras extras]

findUserByName :: String -> [UserExtra] -> Rdio (Either String User)
findUserByName vanityName extras = runRequest $ [("method", "findUser"), ("vanityName", vanityName), mkExtras extras]

removeFriend :: String -> Rdio (Either String Bool)
removeFriend user = runRequest $ [("method", "removeFriend"), ("user", user)]

userFollowers :: String -> Rdio (Either String [User])
userFollowers user = userFollowers' user Nothing Nothing [] Nothing

userFollowers' :: String -> Maybe Int -> Maybe Int -> [UserExtra] -> Maybe String -> Rdio (Either String [User])
userFollowers' user start count extras inCommon =
    runRequest $ [("method", "userFollowers"), ("user", user), mkExtras extras]
                   <+> ("start", start)
                   <+> ("count", count)
                   <+> ("inCommon", inCommon)

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

getPlaybackToken :: Maybe String -> Rdio (Either String String)
getPlaybackToken domain = runRequest $ [("method", "getPlaybackToken")] <+> ("domain", domain)
