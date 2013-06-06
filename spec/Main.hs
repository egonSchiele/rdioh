import Test.Hspec
import Rdioh
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

main = hspec $ do
  describe "test methods" $ do
    it "getAlbumsByUPC" $ do
      testMethod $ getAlbumsByUPC 654979031628 []
    it "getAlbumsForArtist" $ do
      testMethod $ getAlbumsForArtist "r91318" -- Radiohead
    it "getAlbumsForLabel" $ do
      testMethod $ getAlbumsForLabel ""
    it "getArtistsForLabel" $ do
      testMethod $ getArtistsForLabel ""
    it "getTracksByISRC" $ do
      testMethod $ getTracksByISRC "" []
    it "getTracksForArtist" $ do
      testMethod $ getTracksForArtist "r91318"
    it "searchForArtist" $ do
      testMethod $ searchForArtist "Radiohead"
    it "getAlbumsForArtistInCollection" $ do
      testMethod $ getAlbumsForArtistInCollection ""
    it "getAlbumsInCollection'" $ do
      testMethod $ getAlbumsInCollection' (Just "") Nothing Nothing Nothing Nothing []
    it "getArtistsInCollection'" $ do
      testMethod $ getArtistsInCollection' (Just "") Nothing Nothing Nothing Nothing []
    it "getTracksForAlbumInCollection" $ do
      testMethod $ getTracksForAlbumInCollection ""
    it "getTracksForArtistInCollection" $ do
      testMethod $ getTracksForArtistInCollection ""
    it "getTracksInCollection'" $ do
      testMethod $ getTracksInCollection' (Just "") Nothing Nothing Nothing Nothing []
    it "getPlaylists'" $ do
      testMethod $ getPlaylists' (Just "") [] Nothing
    it "getUserPlaylists" $ do
      testMethod $ getUserPlaylists ""
    it "findUserByEmail" $ do
      testMethod $ findUserByEmail "" []
    it "findUserByName" $ do
      testMethod $ findUserByName "" []
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
