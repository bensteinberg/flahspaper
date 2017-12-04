{-# LANGUAGE OverloadedStrings #-}
module FlahspaperFuncSpec (spec) where

import Test.Hspec
import Flahspaper
import Network.Wai
import Network.HTTP.Types

spec :: Spec
spec = do
  describe "Verify that isSlack works" $ do
    it "Slack" $ do
      isSlack (Just "Slackbot") `shouldBe` True
    it "not Slack" $ do
      isSlack (Just "Hello") `shouldBe` False
    it "nothing" $ do
      isSlack Nothing `shouldBe` False

  describe "Verify shareable" $ do
    let r = shareable (Options 12 34) "https://localhost" "1234"
    it "Check status" $ do
      responseStatus r `shouldBe` status200
      let headers = responseHeaders r
      length headers `shouldBe` 1
      fst (head headers) `shouldBe` hContentType
      snd (head headers) `shouldBe` "text/html"
