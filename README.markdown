# Rdioh

## About

This package is a near-complete implementation of [Rdio](http://rdio.com)'s API in Haskell.

## Installation

    cabal install rdioh

## Usage

First you need a key and a secret. You can get both [here](http://developer.rdio.com/member/register).

Then usage is simple:

```haskell
main = do
    result <- runRdio key secret $ getTopChartAlbums
    case result of
      Left err -> putStrLn err
      Right albums -> mapM_ (putStrLn . albumName) albums
```
Everything runs in the `Rdio` Monad, which is just the `ReaderT` monad.

## Authentication

`runRdio` is for methods that don't require user authentication. Use `runRdioWithAuth` for methods that require user authentication, like `createPlaylist`. If you use `runRdioWithAuth`, your user will see a message like so:

    open https://www.rdio.com/oauth/authorize?oauth_token=asdaslkjfsd
    oauth_verifier: 

To complete authentication, he / she will have to open the url, hit 'Allow', and paste the given PIN back into your application.

## Longer Example

List all albums for Radiohead:

```haskell
import Rdioh
import Rdioh.Models
import Control.Monad.IO.Class

main = do
    runRdio key secret $ do
      res1 <- search "Radiohead" "Artist" :: Rdio (Either String [Artist])
      case res1 of
        Left err -> liftIO $ putStrLn err
        Right (radiohead:_) -> do
          res2 <- getAlbumsForArtist (artistKey radiohead)
          case res2 of
            Left err -> liftIO $ putStrLn err
            Right albums -> liftIO $ mapM_ (putStrLn . albumName) albums
```
