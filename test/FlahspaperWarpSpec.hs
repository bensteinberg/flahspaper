module FlahspaperWarpSpec (spec) where

import Flahspaper
import Test.Hspec
import qualified Data.Map.Strict as Map
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Network.Wai.Handler.Warp (run)
import Network.Wreq
import Network.Wreq.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import Control.Lens
import Text.HTML.TagSoup

runApp :: IO ()
runApp = do
  let sm = Map.empty :: SecretMap
  secrets <- newTVarIO sm
  let sopts = Flahspaper.Options 1 2048
  _ <- ($) forkIO $ janitor secrets sopts
  _ <- ($) forkIO $ run 8080 $ app secrets sopts
  putStrLn "  Started server..."

get' :: String -> IO (Response BL.ByteString)
get' path = get $ "http://localhost:8080" ++ path

post' :: Postable a => String -> a -> IO (Response BL.ByteString)
post' path = post $ "http://localhost:8080" ++ path

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
    it "GET /badness" $ do
      r'' <- getWith opts "http://localhost:8080/badness"
      (r'' ^. responseBody) `shouldBe`
        BLC.pack "You are likely to be eaten by a grue"

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
      r'' <- getWith opts $ BLC.unpack url
      (r'' ^. responseStatus . statusCode) `shouldBe` 404
      (r'' ^. responseBody) `shouldBe`
        BLC.pack "You are likely to be eaten by a grue"

    it "Create and retrieve a secret file" $ do
      -- upload a file
      r <- post' "/addfile" (partFile (T.pack "file") "LICENSE")
      let url = getH2 $ r ^. responseBody
      (r ^. responseStatus . statusCode) `shouldBe` 200
      -- retrieve it; confirm that it is the same file?
      r' <- get $ BLC.unpack url
      (r' ^. responseStatus . statusCode) `shouldBe` 200
      -- try to retrieve it again
      r'' <- getWith opts $ BLC.unpack url
      (r'' ^. responseStatus . statusCode) `shouldBe` 404
      (r'' ^. responseBody) `shouldBe`
        BLC.pack "You are likely to be eaten by a grue"

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
      -- wait two seconds
      threadDelay 2000000
      -- try to retrieve it
      r' <- getWith opts $ BLC.unpack url
      (r' ^. responseStatus . statusCode) `shouldBe` 404
      (r' ^. responseBody) `shouldBe`
        BLC.pack "You are likely to be eaten by a grue"

    it "Post file improperly" $ do
      r <- postWith opts "http://localhost:8080/addfile"
           (partFile (T.pack "badfile") "LICENSE")
      (r ^. responseStatus . statusCode) `shouldBe` 400
      (r ^. responseBody) `shouldBe`
        BLC.pack "yuck"

      where getH2 =
              innerText . take 2 . dropWhile (~/= "<h2>") . parseTags
            -- https://stackoverflow.com/a/34290005 plus tweak
            opts = set Network.Wreq.checkResponse
              (Just $ \_ _ -> return ()) defaults
