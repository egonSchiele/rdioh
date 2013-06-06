{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Rdioh.Util where
import qualified Data.URLEncoded as UE
import qualified Data.List.Utils as U
import qualified Text.JSON as J
import Rdioh.Models

-- convert a list of parameters to a string that can be passed via GET/POST
toParams :: [(String, String)] -> String
toParams = show . UE.importList

(<+>) :: Param p => [(String, String)] -> (String, Maybe p) -> [(String, String)]
arr <+> (_, Nothing) = arr
arr <+> (str, (Just param)) = arr ++ [(str, toParam param)]

class Param a where
    toParam :: a -> String

instance Param Bool where
    toParam True = "true"
    toParam False = "false"

instance Param Int where
    toParam val = show val

instance Param String where
    toParam val = val

instance Param [String] where
    toParam list = U.join "," list

instance Param PlaylistType where
    toParam = show

instance Param CollaborationMode where
    toParam NoCollaboration = "0"
    toParam CollaborationWithAll = "1"
    toParam CollaborationWithFollowed = "2"

instance Param Scope where
    toParam = show

instance Param Timeframe where
    toParam = show

-- class RdioType a where
--     typeName :: String

-- instance RdioType Album where
--   typeName = "Album"

-- instance RdioType Artist where
--   typeName = "Artist"

-- instance RdioType Label where
--   typeName = "Label"

-- instance RdioType Track where
--   typeName = "Track"

-- instance RdioType Playlist where
--   typeName = "Playlist"

-- instance RdioType UserPlaylists where
--   typeName = "UserPlaylists"

-- instance RdioType User where
--   typeName = "User"

-- instance RdioType CollectionAlbum where
--   typeName = "CollectionAlbum"

-- instance RdioType CollectionArtist where
--   typeName = "CollectionArtist"

-- instance RdioType LabelStation where
--   typeName = "LabelStation"

-- instance RdioType ArtistStation where
--   typeName = "ArtistStation"

-- instance RdioType HeavyRotationUserStation where
--   typeName = "HeavyRotationUserStation"

-- instance RdioType ArtistTopSongsStation where
--   typeName = "ArtistTopSongsStation"

-- instance RdioType UserCollectionStation where
--   typeName = "UserCollectionStation"

-- instance RdioType SearchResults where
--   typeName = "SearchResults"

-- instance RdioType Activity where
--   typeName = "Activity"
