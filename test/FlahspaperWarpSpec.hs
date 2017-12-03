module FlahspaperWarpSpec (spec) where

import Flahspaper
import Test.Hspec
import qualified Data.Map.Strict as Map
import Control.Concurrent (forkIO)
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
  _ <- ($) forkIO $ janitor secrets
  _ <- ($) forkIO $ run 8080 $ app secrets
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
      (r'' ^. responseBody) `shouldBe` BLC.pack "You are likely to be eaten by a grue"
  describe "Round-trip testing" $ do
    it "Create and retrieve a secret" $ do
      -- create a secret
      r <- post' "/add" [BC.pack "secret" := "Hello"]
      let url = getUrl $ r ^. responseBody
      (r ^. responseStatus . statusCode) `shouldBe` 200
      -- retrieve the secret
      r' <- get $ BLC.unpack url
      (r' ^. responseStatus . statusCode) `shouldBe` 200
      (r' ^. responseBody) `shouldBe` BLC.pack "Hello"
      r'' <- getWith opts $ BLC.unpack url
      (r'' ^. responseBody) `shouldBe` BLC.pack "You are likely to be eaten by a grue"
    it "Create and retrieve a secret file" $ do
      r <- post' "/addfile" (partFile (T.pack "file") "README.md")
      let url = getUrl $ r ^. responseBody
      (r ^. responseStatus . statusCode) `shouldBe` 200
      r' <- get $ BLC.unpack url
      (r' ^. responseStatus . statusCode) `shouldBe` 200

      r'' <- getWith opts $ BLC.unpack url
      (r'' ^. responseBody) `shouldBe` BLC.pack "You are likely to be eaten by a grue"
      where getUrl =
              innerText . take 2 . dropWhile (~/= "<h2 id=url>") . parseTags
            -- https://stackoverflow.com/a/34290005 needs a tweak
            opts = set Network.Wreq.checkResponse (Just $ \_ _ -> return ()) defaults
