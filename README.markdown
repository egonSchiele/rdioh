# Rdioh

## About
This is a Haskell package for accessing [Rdio](http://rdio.com)'s API with OAuth.

## OAuth

Rdio supports 2-legged and 3-legged OAuth. 2-legged OAuth only requires a key and a secret. You can get both by [registering for a Mashery account](http://developer.rdio.com/member/register).

3-legged OAuth requires user input as well. If you are using 3-legged OAuth, when the user runs your script he / she will see a message like this:

    open https://www.rdio.com/oauth/authorize?oauth_token=asdaslkjfsd
    oauth_verifier: 

To complete 3-legged authentication, he / she will have to open the url, hit 'Allow', and paste the given PIN back into your application.

## Examples


Getting Information On An Album (2-legged OAuth):

	import Rdioh
	import Control.Monad.Reader

	key = "[YOUR KEY]"
	secret = "[YOUR SECRET]"

	req = get ["a184236"] Nothing Nothing
	main = runReaderT req (twoLegToken key secret) >>= (putStrLn . show)

Returns:

	{"status" : "ok", "result" : {"a184236" : {"baseIcon" : "album/c/a/f/000000000002cfac/square-200.jpg", "releaseDate" : "2005-11-08", "displayDate" : "Nov  8, 2005", "duration" : 2558 % 1, "isClean" : False, "shortUrl" : "http://rd.io/x/Qj5AkO8", "canStream" : True, "embedUrl" : "http://rd.io/e/Qj5AkO8", "type" : "a", "price" : "9.99", "key" : "a184236", "icon" : "http://media.rd.io/album/c/a/f/000000000002cfac/square-200.jpg", "canSample" : True, "name" : "Bleach", "isExplicit" : False, "artist" : "Nirvana", "url" : "/artist/Nirvana/album/Bleach/", "artistKey" : "r89765", "length" : 13 % 1, "trackKeys" : ["t2221202", "t2221252", "t2221287", "t2221335", "t2221373", "t2221437", "t2221498", "t2221554", "t2221624", "t2221671", "t2221740", "t2221839", "t2221913"], "canTether" : True, "artistUrl" : "/artist/Nirvana/"}}}

Add a Track To The Current User's Collection (3-legged OAuth):

	import Rdioh
	import Control.Monad.Reader

	key = "[YOUR KEY]"
	secret = "[YOUR SECRET]"

	req = addToCollection ["t12032557"]
	main = do
		tok <- threeLegToken key secret
		runReaderT req tok >>= (putStrLn . show)

Returns:

	{"status" : "ok", "result" : True}

* Rdioh uses the [Reader](http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/Control-Monad-Reader.html) Monad. In the above examples, all calls to the Rdio API are done in the `req` function. We call the function with an OAuth token like so: `runReaderT req mytoken`. [Here's a good tutorial on the Reader Monad](http://www.maztravel.com/haskell/readerMonad.html).

* All optional parameters are of type `Maybe`. You can leave them out by passing in `Nothing`.

* All responses are RdioResult objects, which is an ADT defined as

    data RdioResult = RdioDict {rdioDictValue :: [(String, RdioResult)]} | RdioArray {rdioArrayValue :: [RdioResult]} | RdioString {rdioStringValue :: String} | RdioRational {rdioRationalValue :: Rational} | RdioBool {rdioBoolValue :: Bool} | RdioNull

This reflects the data types allowed in JSON: Dictionaries, Arrays, Strings, Rationals, Booleans and Null values.

We can access pieces of the response like so:

	r = get ["a184236"] Nothing Nothing

	main = do
		x <- runReaderT r (twoLegToken key secret)
		(putStrLn . show) $ rdioDictValue x !! 1 

(Assuming the response was:)

	{"status" : "ok", "result" : {"a184236" : {"baseIcon" : "album/c/a/f/000000000002cfac/square-200.jpg", "releaseDate" : "2005-11-08", "displayDate" : "Nov  8, 2005", "duration" : 2558 % 1, "isClean" : False, "shortUrl" : "http://rd.io/x/Qj5AkO8", "canStream" : True, "embedUrl" : "http://rd.io/e/Qj5AkO8", "type" : "a", "price" : "9.99", "key" : "a184236", "icon" : "http://media.rd.io/album/c/a/f/000000000002cfac/square-200.jpg", "canSample" : True, "name" : "Bleach", "isExplicit" : False, "artist" : "Nirvana", "url" : "/artist/Nirvana/album/Bleach/", "artistKey" : "r89765", "length" : 13 % 1, "trackKeys" : ["t2221202", "t2221252", "t2221287", "t2221335", "t2221373", "t2221437", "t2221498", "t2221554", "t2221624", "t2221671", "t2221740", "t2221839", "t2221913"], "canTether" : True, "artistUrl" : "/artist/Nirvana/"}}}

That prints out the result part of the response. This is ugly syntax, so two convenience functions are provided:

`!` to index into Dictionaries,

`.!` to index into Arrays.

Now we can print out the result like this:

	(putStrLn . show) x ! "result"

Much cleaner.

We can chain these functions to print the name of the album as well as the first track on the album:

    putStrLn . rdioStringValue $ x ! "result" ! "a184236" ! "name"
    putStrLn . rdioStringValue $ x ! "result" ! "a184236" ! "trackKeys" .! 0

Finally, you can get all the keys or values from an RdioDict by using `keys` and `values` respectively.

## Putting It All Together

Here's a script that gets newly released albums and adds the first album to the current user's collection:

	import Rdioh
	import Control.Monad.Reader

	key = "[YOUR KEY]"
	secret = "[YOUR SECRET]"

	r = do
		n <- getNewReleases (Just THIS_WEEK_TIME) Nothing Nothing Nothing
		let tracks = map rdioStringValue $ rdioArrayValue $ n ! "result" .! 0 ! "trackKeys"
		addToCollection tracks

	main = do
		tok <- threeLegToken key secret
		x <- runReaderT r tok
		putStrLn . show $ x

## License
MIT Licensed. Copyright 2011- [Aditya Bhargava](http://www.wefoundland.com).
