module Rdioh.Auth where
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.PercentEncoding
import Data.Maybe
import Data.List
import Control.Monad.IO.Class

reqUrl = fromJust . parseURL $ "http://api.rdio.com/oauth/request_token"
accUrl = fromJust . parseURL $ "http://api.rdio.com/oauth/access_token"
authUrl = ("https://www.rdio.com/oauth/authorize?oauth_token="++) . findWithDefault ("oauth_token","ERROR") . oauthParams
srvUrl payload = (fromJust . parseURL $ "http://api.rdio.com/1/") { method = POST
                                                          , reqPayload = payload
                                                          , reqHeaders = fromList [("content-type", "application/x-www-form-urlencoded")]
                                                          }

app key secret = Application key secret OOB

-- | Takes: a key and a secret. Returns a two-legged auth token
twoLegToken :: String -> String -> Token
twoLegToken key secret = fromApplication (app key secret)

-- | Takes: a key and a secret. Does three-legged auth and returns an auth token.
threeLegToken :: MonadIO m => String -> String -> m Token
threeLegToken key secret = runOAuthM (twoLegToken key secret) $ do
    signRq2 HMACSHA1 Nothing reqUrl >>= oauthRequest CurlClient
    cliAskAuthorization authUrl
    signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest CurlClient
