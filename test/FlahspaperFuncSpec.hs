{-# LANGUAGE OverloadedStrings #-}
module FlahspaperFuncSpec (spec) where

import Test.Hspec
import Flahspaper
import Data.Time
import Network.Wai
import Network.HTTP.Types
-- import qualified Data.ByteString as B


spec :: Spec
spec = do
  describe "Make a new Secret" $ do
    it "Data Secret" $ do
      s <- newSecret Data "some data" ""
      now <- getCurrentTime
      (diffUTCTime now (secretTime s) < 1) `shouldBe` True
      secretType s `shouldBe` Data
      length (secretId s) > 0 `shouldBe` True
      secretName s `shouldBe` ""
      secretData s `shouldBe` "some data"
    it "File Secret" $ do
      s <- newSecret File "some data" "file.txt"
      now <- getCurrentTime
      (diffUTCTime now (secretTime s) < 1) `shouldBe` True
      secretType s `shouldBe` File
      length (secretId s) > 0 `shouldBe` True
      secretName s `shouldBe` "file.txt"
      secretData s `shouldBe` "some data"

  describe "Verify that isSlack works" $ do
    it "Slack" $ do
      isSlack (Just "Slackbot") `shouldBe` True
    it "not Slack" $ do
      isSlack (Just "Hello") `shouldBe` False
    it "nothing" $ do
      isSlack Nothing `shouldBe` False

  describe "Verify shareable" $ do
    let r = shareable "https://localhost" "1234"
    it "Check status" $ do
      responseStatus r `shouldBe` status200
      let headers = responseHeaders r
      length headers `shouldBe` 1
      fst (head headers) `shouldBe` hContentType
      snd (head headers) `shouldBe` "text/html"
