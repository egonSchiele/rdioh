import Test.Hspec
import Rdioh
import Rdioh.Models
import System.IO.Unsafe
import Data.String.Utils
import Data.Aeson
import qualified Debug.Trace as D

key = strip . unsafePerformIO . readFile $ "key"
secret = strip . unsafePerformIO . readFile $ "secret"

testMethod :: (Show a, FromJSON a) => Rdio (Either String a) -> Bool
testMethod meth = unsafePerformIO $ do
    result <- runRdio key secret meth
    case result of
      Left err -> fail err
      Right x -> return True

radiohead = "r91318"
me = "s361565"
emi = "l202397"

okcomputer = "a171828"

main = hspec $ do
  describe "test methods" $ do
    it "getAlbumsByUPC" $ do
      testMethod $ getAlbumsByUPC 654979031628 []
    it "getAlbumsForArtist" $ do
      testMethod $ getAlbumsForArtist radiohead
    it "getAlbumsForLabel" $ do
      testMethod $ getAlbumsForLabel emi
    it "getArtistsForLabel" $ do
      testMethod $ getArtistsForLabel emi
    it "getTracksByISRC" $ do
      testMethod $ getTracksByISRC "" []
    it "getTracksForArtist" $ do
      testMethod $ getTracksForArtist radiohead
    it "search" $ do
      testMethod $ (search "Radiohead" "Artist" :: Rdio (Either String [Label]))
    it "getAlbumsForArtistInCollection'" $ do
      testMethod $ getAlbumsForArtistInCollection' radiohead (Just me) [] Nothing -- me
    it "getAlbumsInCollection'" $ do
      testMethod $ getAlbumsInCollection' (Just me) Nothing Nothing Nothing Nothing []
    it "getArtistsInCollection'" $ do
      testMethod $ getArtistsInCollection' (Just me) Nothing Nothing Nothing Nothing []
    it "getTracksForAlbumInCollection'" $ do
      testMethod $ getTracksForAlbumInCollection' okcomputer (Just me) []
    it "getTracksForArtistInCollection'" $ do
      testMethod $ getTracksForArtistInCollection' radiohead (Just me) []
    it "getTracksInCollection'" $ do
      testMethod $ getTracksInCollection' (Just me) Nothing Nothing Nothing Nothing []
    it "getPlaylists'" $ do
      testMethod $ getPlaylists' (Just me) [] Nothing
    it "getUserPlaylists" $ do
      testMethod $ getUserPlaylists me
    it "findUserByEmail" $ do
      testMethod $ findUserByEmail "bluemangroupie@gmail.com" []
    it "findUserByName" $ do
      testMethod $ findUserByName "egonschiele" []
    it "getNewReleases" $ do
      testMethod $ getNewReleases
    it "getTopChartArtists" $ do
      testMethod $ getTopChartArtists
    it "getTopChartAlbums" $ do
      testMethod $ getTopChartAlbums
    it "getTopChartTracks" $ do
      testMethod $ getTopChartTracks
    it "getTopChartPlaylists" $ do
      testMethod $ getTopChartPlaylists
