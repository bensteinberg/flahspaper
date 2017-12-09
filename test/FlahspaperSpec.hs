module FlahspaperSpec (spec) where

import Flahspaper
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as TQ
import Test.QuickCheck.Instances ()
import qualified Data.Map.Strict as Map
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Network.Wai.Handler.Warp (run)
import Network.Wreq
import Network.Wreq.Types
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import Control.Lens
import Text.HTML.TagSoup
import Network.HTTP.Types (hUserAgent)
import Data.Text.Encoding as E

runApp :: IO ()
runApp = do
  let sm = Map.empty :: SecretMap
  secrets <- newTVarIO sm
  let sopts = Flahspaper.Options 0.1 2048 5000
  -- syntax to suppress a warning:
  -- A do-notation statement discarded a result of type
  --  ‘GHC.Conc.Sync.ThreadId’
  _ <- ($) forkIO $ janitor secrets sopts
  _ <- ($) forkIO $ run 8080 $ app secrets sopts
  putStrLn "  Started server..."

get' :: String -> IO (Response BL.ByteString)
get' path = get $ "http://localhost:8080" ++ path

get'' :: String -> IO (Response BL.ByteString)
get'' = getWith opts

post' :: Postable a => String -> a -> IO (Response BL.ByteString)
post' path = post $ "http://localhost:8080" ++ path

grue :: BLC.ByteString
grue = BLC.pack "You are likely to be eaten by a grue"

yuck :: BLC.ByteString
yuck = BLC.pack "yuck"

getH2 :: BLC.ByteString -> BLC.ByteString
getH2 = innerText . take 2 . dropWhile (~/= "<h2>") . parseTags

-- https://stackoverflow.com/a/34290005 plus tweak
opts :: Network.Wreq.Types.Options
opts = set Network.Wreq.checkResponse (Just $ \_ _ -> return ()) defaults

slackopts :: Network.Wreq.Types.Options
slackopts = opts & header hUserAgent .~ [BC.pack "Slackbot"]

retrieve :: T.Text -> IO T.Text
retrieve s = do
  -- create a secret
  r <- post' "/add" [BC.pack "secret" := s]
  let url = getH2 $ r ^. responseBody
  -- retrieve the secret
  r' <- get $ BLC.unpack url
  return $ E.decodeUtf8 $ B.concat $ BL.toChunks $ r' ^. responseBody

retrieveAndRetry :: T.Text -> IO (T.Text, Int)
retrieveAndRetry s = do
  -- create a secret
  r <- post' "/add" [BC.pack "secret" := s]
  let url = getH2 $ r ^. responseBody
  -- retrieve the secret
  r' <- get $ BLC.unpack url
  -- try to retrieve it again
  r'' <- get'' $ BLC.unpack url
  return (E.decodeUtf8 $ B.concat $ BL.toChunks (r' ^. responseBody),
          r'' ^. responseStatus . statusCode)

postAndWait :: T.Text -> IO Int
postAndWait s = do
  -- create a secret
  r <- post' "/add" [BC.pack "secret" := s]
  let url = getH2 $ r ^. responseBody
  -- wait
  threadDelay 100000
  -- try to retrieve it
  r' <- get'' $ BLC.unpack url
  return $ r' ^. responseStatus . statusCode

prop_secretMatch :: T.Text -> Property
prop_secretMatch s = TQ.monadicIO $ do
  s' <- TQ.run $ retrieve s
  TQ.assert $ s == s'

prop_secretMatchAndRetry :: T.Text -> Property
prop_secretMatchAndRetry s = TQ.monadicIO $ do
  (s', status) <- TQ.run $ retrieveAndRetry s
  TQ.assert $ s == s'
  TQ.assert $ status == 404

prop_secretLapse :: T.Text -> Property
prop_secretLapse s = TQ.monadicIO $ do
  status <- TQ.run $ postAndWait s
  TQ.assert $ status == 404

spec :: Spec
spec = beforeAll runApp $ do
  describe "Test GET routes" $ do
    it "GET /" $ do
      r <- get' "/"
      (r ^. responseStatus . statusCode) `shouldBe` 200
    it "GET /add" $ do
      r <- get' "/add"
      (r ^. responseStatus . statusCode) `shouldBe` 200
    it "GET /addfile" $ do
      r <- get' "/addfile"
      (r ^. responseStatus . statusCode) `shouldBe` 200
    it "GET /favicon.ico" $ do
      r <- get'' "http://localhost:8080/favicon.ico"
      (r ^. responseStatus . statusCode) `shouldBe` 404
    it "GET /badness" $ do
      r <- get'' "http://localhost:8080/badness"
      (r ^. responseBody) `shouldBe` grue

  describe "Round-trip testing" $ do
    it "Create and retrieve a secret" $ do
      -- create a secret
      r <- post' "/add" [BC.pack "secret" := "Hello"]
      let url = getH2 $ r ^. responseBody
      (r ^. responseStatus . statusCode) `shouldBe` 200
      -- retrieve the secret
      r' <- get $ BLC.unpack url
      (r' ^. responseStatus . statusCode) `shouldBe` 200
      (r' ^. responseBody) `shouldBe` BLC.pack "Hello"
      -- try to retrieve it again
      r'' <- get'' $ BLC.unpack url
      (r'' ^. responseStatus . statusCode) `shouldBe` 404
      (r'' ^. responseBody) `shouldBe` grue

    it "Create and retrieve a secret file" $ do
      -- upload a file
      r <- post' "/addfile" (partFile (T.pack "file") "LICENSE")
      original <- readFile "LICENSE"
      let url = getH2 $ r ^. responseBody
      (r ^. responseStatus . statusCode) `shouldBe` 200
      -- retrieve it; confirm that it is the same file
      r' <- get $ BLC.unpack url
      (r' ^. responseStatus . statusCode) `shouldBe` 200
      original == BLC.unpack (r' ^. responseBody) `shouldBe` True
      -- try to retrieve it again
      r'' <- get'' $ BLC.unpack url
      (r'' ^. responseStatus . statusCode) `shouldBe` 404
      (r'' ^. responseBody) `shouldBe` grue

  describe "Test error conditions" $ do
    it "Try to upload too large a file" $ do
      r <- post' "/addfile" (partFile (T.pack "file") "README.md")
      (r ^. responseStatus . statusCode) `shouldBe` 200
      let msg = getH2 $ r ^. responseBody
      msg `shouldBe` BLC.pack "Upload too large."

    it "Make a secret and let it lapse" $ do
      -- create a secret
      r <- post' "/add" [BC.pack "secret" := "Hello"]
      let url = getH2 $ r ^. responseBody
      (r ^. responseStatus . statusCode) `shouldBe` 200
      -- wait
      threadDelay 100000
      -- try to retrieve it
      r' <- get'' $ BLC.unpack url
      (r' ^. responseStatus . statusCode) `shouldBe` 404
      (r' ^. responseBody) `shouldBe` grue

    it "Post secret improperly" $ do
      r <- postWith opts "http://localhost:8080/add"
           [BC.pack "badsecret" := "Hello"]
      (r ^. responseStatus . statusCode) `shouldBe` 400
      (r ^. responseBody) `shouldBe` yuck

    it "Post file improperly" $ do
      r <- postWith opts "http://localhost:8080/addfile"
           (partFile (T.pack "badfile") "LICENSE")
      (r ^. responseStatus . statusCode) `shouldBe` 400
      (r ^. responseBody) `shouldBe` yuck

    it "Connect from Slack" $ do
      r <- getWith slackopts "http://localhost:8080/"
      (r ^. responseStatus . statusCode) `shouldBe` 404

  describe "QuickCheck secret creation" $ do
    it "Secrets match" $
      property prop_secretMatch
    it "Secrets match, can't retrieve again" $
      property prop_secretMatchAndRetry
    it "Secrets lapse" $
      property prop_secretLapse

  describe "Test function(s)" $ do
    it "isSlack Nothing" $ do
      isSlack Nothing `shouldBe` False
    it "isSlack Something" $ do
      isSlack (Just $ BC.pack "Hello") `shouldBe` False
    it "isSlack Slackbot" $ do
      isSlack (Just $ BC.pack "Slackbot") `shouldBe` True
