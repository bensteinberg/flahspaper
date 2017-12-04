{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Flahspaper where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Request (guessApproot)
import Network.Wai.Parse
import Data.Time
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Crypto.Random.DRBG
import Numeric (showHex)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Text.InterpolatedString.Perl6 (q, qc)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import System.Time.Utils (renderSecs)
import qualified Data.ByteString.UTF8 as BSU

data Type = File | Data deriving Eq

data Secret =
  Secret { secretId   :: String
         , secretType :: Type
         , secretTime :: UTCTime
         , secretData :: B.ByteString
         , secretName :: B.ByteString }

type SecretMap = Map.Map String Secret

type Secrets = TVar SecretMap

data Options =
  Options { secretExpiration  :: NominalDiffTime
          , secretMaxFileSize :: Int }

shareable :: Options -> B.ByteString -> String -> Response
shareable sopts approot key = responseLBS
                             status200
                             [("Content-Type", "text/html")] $
                             BL.concat [lackofstyle, shareform, endofstyle]
  where renderS = renderSecs . round :: NominalDiffTime -> String
        shareform = [qc|
<p>share this link (do not click!):</p>
<h2 id="url">{approot}/{key}</h2>
<p>THIS LINK WILL EXPIRE IN {renderS $ secretExpiration sopts}</p>
<p><a href="/">Share Another Secret</a></p>
|]

janitor :: Secrets -> Options -> IO ()
janitor secrets sopts = forever $ do
  threadDelay 1000000
  now <- getCurrentTime  
  forkIO $ atomically $ sweep secrets sopts now

-- does overwriting a TVar really destroy the original contents?
sweep :: Secrets -> Options -> UTCTime -> STM ()
sweep secrets sopts now = do
  secretMap <- readTVar secrets
  let secretMap' = Map.filter
                   (\x -> diffUTCTime now (secretTime x) <
                     secretExpiration sopts)
                   secretMap
  writeTVar secrets secretMap'

newSecret :: Type -> B.ByteString -> B.ByteString -> IO Secret
newSecret stype payload name = do
  key <- genRandPathString
  t  <- getCurrentTime
  return $ Secret key stype t payload name

-- from https://stackoverflow.com/a/8416189/4074877, enhanced by hlint
prettyPrint :: B.ByteString -> String
prettyPrint = concatMap (`showHex` "") . B.unpack

-- leave the Gen in the function
-- better to pass it around with Control.Monad.Random?
-- check for existence of key in secrets?
genRandPathString :: IO String
genRandPathString = do
  gen <- newGenIO :: IO CtrDRBG
  let Right (randomBytes, newGen) = genBytes 32 gen
  return $ prettyPrint randomBytes

isSlack :: Maybe B.ByteString -> Bool
isSlack Nothing = False
isSlack (Just useragent) = B.isInfixOf "Slack" useragent

insertAndShare :: Secret ->
                  Secrets ->
                  Options ->
                  B.ByteString ->
                  (Response -> IO ResponseReceived) ->
                  IO ResponseReceived
insertAndShare s secrets sopts approot respond = do
  atomically $ do
    secretMap <- readTVar secrets
    let secretMap' = Map.insert (secretId s) s secretMap
    writeTVar secrets secretMap'
  respond $ shareable sopts approot (secretId s)

lookupSecret :: String -> Secrets -> IO (Maybe Secret)
lookupSecret key secrets =
  atomically $ do
    secretMap <- readTVar secrets
    let (secretMap', secretMap'') =
          Map.partitionWithKey (\k _ -> k == key) secretMap
    writeTVar secrets secretMap''
    return $ if not (null $ Map.toList secretMap')
             then Just (secretMap' Map.! key)
             else Nothing

returnSecretOrError :: Maybe Secret ->
                       (Response -> IO ResponseReceived) ->
                       IO ResponseReceived
returnSecretOrError Nothing respond =
  respond $ responseLBS
  status404
  [("Content-Type", "text/plain")]
  "You are likely to be eaten by a grue"
returnSecretOrError (Just s) respond =
  if secretType s == File
  then respond $ responseLBS
       status200
       [("Content-Type", "application/octet-stream"),
        ("Content-Disposition",
         B.concat ["attachment; filename=", secretName s])] $
       BL.fromStrict (secretData s)
  else respond $ responseLBS
       status200
       [("Content-Type", "text/plain")] $
       BL.fromStrict (secretData s)

app :: Secrets -> Options -> Application
app secrets sopts request respond
  -- do not allow Slack to expand links
  -- Test.Hspec.Wai does not get here -- why?
  | isSlack $ requestHeaderUserAgent request =
    respond $ responseLBS
    status404
    [("Content-Type", "text/plain")]
    BL.empty
  -- here are the fixed GET routes
  | pm == ("/favicon.ico", g) = respond $ responseLBS
                                status404
                                [("Content-Type", "text/plain")]
                                BL.empty
  -- the first match appears to be necessary for Test.Hspec.Wai,
  -- and the second appears to be necessary for serving via Warp...
  | pm == ("", g) || pm == ("/", g) = respond $ responseLBS
                                      status200
                                      [("Content-Type", "text/html")] $
                                      BL.concat
                                      [lackofstyle, index, endofstyle]
  | pm == ("/add", g)         = respond $ responseLBS
                                status200
                                [("Content-Type", "text/html")] $
                                BL.concat
                                [lackofstyle, inputtextform, endofstyle]
  | pm == ("/addfile", g)     = respond $ responseLBS
                                status200
                                [("Content-Type", "text/html")] $
                                BL.concat
                                [lackofstyle, inputfileform, endofstyle]
  -- here are the two POST routes
  | pm == ("/add", p) = do
      parsed <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd r
      let kv = filter (\x -> fst x == "secret") (fst parsed)
      if not (null kv)
      then do
        let payload = snd $ head kv
        s <- newSecret Data payload B.empty
        insertAndShare s secrets sopts approot respond
      else respond $ responseLBS
           status400
           [("Content-Type", "text/plain")]
           "yuck"
  | pm == ("/addfile", p) = do
      parsed <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd r
      let kv = filter (\x -> fst x == "file") (snd parsed)
      if not (null kv)
      then do
        let fileinfo = snd $ head kv
            filecontent = BL.toStrict $ fileContent fileinfo
        if B.length filecontent < secretMaxFileSize sopts
        then do
          s <- newSecret File filecontent (fileName fileinfo)
          insertAndShare s secrets sopts approot respond
        else respond $ responseLBS
             status200
             [("Content-Type", "text/html")] $
             BL.concat
             [lackofstyle, uploaderror, endofstyle]
      else respond $ responseLBS
           status400
           [("Content-Type", "text/plain")]
           "yuck"
  -- and here is the residue
  | otherwise = do
      let key = tail $ BSU.toString $ fst pm
      ms <- lookupSecret key secrets
      returnSecretOrError ms respond
  where r  = request
        g  = methodGet
        p  = methodPost
        pm = (rawPathInfo r, requestMethod r)
        approot = guessApproot r

lackofstyle = [q|
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <title>flahspaper</title>
  </head>
  <style>
  * { font-family: "Raleway", "HelveticaNeue", "Helvetica Neue", Helvetica, Arial, sans-serif; }
  </style>  
  <body>
  <div style="text-align:center; top: 25px;">
|]
  
endofstyle = [q|
  </div>
  </body>
</html>
|]

index = [q|
<p><a href="/add">Share a TEXT secret</a>.</p>
<p><a href="/addfile">Share a secret FILE</a>.</p>
|]

inputtextform = [q|
<form action="/add" method="POST">
<textarea name="secret" rows="20" cols="80"></textarea>
<br />
<input type=submit>
</form>
|]

inputfileform = [q|
<form action="/addfile" method="POST" enctype="multipart/form-data">
<input type="file" name="file" id="file">
<input type=submit>
</form>
|]

uploaderror = [q|
<h2 id="msg">Upload too large.</h2>
<p><a href="/">Share Another Secret</a></p>
|]
