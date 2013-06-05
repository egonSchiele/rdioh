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

addToCollection :: [String] -> Rdioh (Either String Object)
addToCollection keys = runRequest $ [("method", "addToCollection"), ("keys", U.join "," keys)]

getAlbumsForArtistInCollection :: String -> Rdioh (Either String [Albums])
getAlbumsForArtistInCollection artist = getAlbumsForArtistInCollection' artist Nothing [] Nothing

getAlbumsForArtistInCollection' :: String -> Maybe String -> [AlbumExtra] -> SortOrder

getOfflineTracks :: Rdioh (Either String [Track])
getOfflineTracks = getOfflineTracks' Nothing Nothing []

getOfflineTracks' :: Maybe Int -> Maybe Int -> [TrackExtra] -> Rdioh (Either String [Track])
getOfflineTracks' start count extras = 
    runRequest $ [("method", "getOfflineTracks"), mkExtras extras]
                  <+> ("start", start)
                  <+> ("count", count)

