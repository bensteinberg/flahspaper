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
import Control.Lens

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
    -- it "GET /badness" $ do
    --   r <- getWith (set Network.Wreq.checkResponse (Just $ \_ _ _ -> Nothing) defaults) "http://localhost:8080/badness"
    --   r <- get' "/badness"
    --   (r ^. responseStatus . statusCode) `shouldBe` 404
    --   evaluate (get' "/badness") `shouldThrow` HttpException
  describe "Round-trip testing" $ do
    it "Create a secret and get a URL" $ do
      pending
    it "Retrieve the secret" $ do
      pending
    it "Confirm the secret can no longer be retrieved"
      pending
    it "Create a secret from a file and get a URL" $ do
      pending
    it "Retrieve the file" $ do
      pending
    it "Confirm the file can no longer be retrieved"
      pending    
