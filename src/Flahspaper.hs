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

data Type = File | Data deriving (Eq, Show)

data Secret =
  Secret { secretId   :: String
         , secretType :: Type
         , secretTime :: UTCTime
         , secretData :: B.ByteString
         , secretName :: B.ByteString } deriving (Eq, Show)

type SecretMap = Map.Map String Secret

type Secrets = TVar SecretMap

hours = 24
expiration = hours * 60 * 60  -- in seconds
maxFileSize = 10485760

shareable :: B.ByteString -> String -> Response
shareable approot key = responseLBS
                      status200
                      [("Content-Type", "text/html")] $
                      BL.concat $ [lackofstyle, shareform, endofstyle]
  where renderS = renderSecs . round :: NominalDiffTime -> String
        shareform = [qc|
<p>share this link (do not click!):</p>
<h2>{approot}/{key}</h2>
<p>THIS LINK WILL EXPIRE IN {renderS expiration}</p>
<p><a href="/">Share Another Secret</a></p>
|]

janitor :: Secrets -> IO ()
janitor secrets = forever $ do
  threadDelay 1000000
  now <- getCurrentTime  
  forkIO $ atomically $ sweep secrets now

-- does overwriting a TVar really destroy the original contents?
sweep :: Secrets -> UTCTime -> STM ()
sweep secrets now = do
  secretMap <- (readTVar secrets)
  let secretMap' = Map.filter
                   (\x -> diffUTCTime now (secretTime x) < expiration)
                   secretMap
  writeTVar secrets secretMap'

newSecret :: Type -> B.ByteString -> B.ByteString -> IO Secret
newSecret stype payload name = do
  key <- genRandPathString
  t  <- getCurrentTime
  return $ Secret key stype t payload name

-- from https://stackoverflow.com/a/8416189/4074877
prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

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
                  B.ByteString ->
                  (Response -> IO ResponseReceived) ->
                  IO ResponseReceived
insertAndShare s secrets approot respond = do
  atomically $ do
    secretMap <- (readTVar secrets)
    let secretMap' = Map.insert (secretId s) s secretMap
    writeTVar secrets secretMap'
  respond $ shareable approot (secretId s)

lookupSecret :: String -> Secrets -> IO (Maybe Secret)
lookupSecret key secrets = do
  s <- atomically $ do
    secretMap <- (readTVar secrets)
    let (secretMap', secretMap'') =
          Map.partitionWithKey (\k _ -> k == key) secretMap
    writeTVar secrets secretMap''
    return $ if (Prelude.length $ Map.toList secretMap') > 0
             then Just (secretMap' Map.! key)
             else Nothing
  return s

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
         B.concat $ ["attachment; filename=", secretName s])] $
       BL.fromStrict (secretData s)
  else respond $ responseLBS
       status200
       [("Content-Type", "text/plain")] $
       BL.fromStrict (secretData s)

app :: Secrets -> Application
app secrets request respond
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
                                      BL.concat $
                                      [lackofstyle, index, endofstyle]
  | pm == ("/add", g)         = respond $ responseLBS
                                status200
                                [("Content-Type", "text/html")] $
                                BL.concat $
                                [lackofstyle, inputtextform, endofstyle]
  | pm == ("/addfile", g)     = respond $ responseLBS
                                status200
                                [("Content-Type", "text/html")] $
                                BL.concat $
                                [lackofstyle, inputfileform, endofstyle]
  -- here are the two POST routes
  | pm == ("/add", p) = do
      parsed <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd r
      let kv = filter (\x -> fst x == "secret") (fst parsed)
      if length kv > 0
      then do
        let payload = snd $ head $ kv
        s <- newSecret Data payload B.empty
        insertAndShare s secrets approot respond
      else respond $ responseLBS
           status400
           [("Content-Type", "text/plain")]
           "yuck"
  | pm == ("/addfile", p) = do
      parsed <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd r
      let kv = filter (\x -> fst x == "file") (snd parsed)
      if length kv > 0
      then do
        let fileinfo = snd $ head $ kv
            filecontent = BL.toStrict $ fileContent fileinfo
        if (B.length filecontent) < maxFileSize
        then do
          s <- newSecret File filecontent (fileName fileinfo)
          insertAndShare s secrets approot respond
        else respond $ responseLBS
             status200
             [("Content-Type", "text/html")] $
             BL.concat $
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
<h2>Upload too large.</h2>
<p><a href="/">Share Another Secret</a></p>
|]
