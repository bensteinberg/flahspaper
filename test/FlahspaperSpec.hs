{-# LANGUAGE OverloadedStrings #-}
module FlahspaperSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Flahspaper
import qualified Data.Map.Strict as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Network.Wai (Application)
-- import Network.HTTP.Types
-- import Data.Time

myApp :: IO Application
myApp = do
  let sm = Map.empty :: SecretMap
  secrets <- newTVarIO sm
  let sopts = Options (24 * 60 * 60) 10485760
  -- syntax to suppress a warning:
  -- A do-notation statement discarded a result of type
  --  ‘GHC.Conc.Sync.ThreadId’
  _ <- ($) forkIO $ janitor secrets sopts
  return $ app secrets sopts

spec :: Spec
spec = with myApp $ do
  describe "Test routes" $ do
    it "GET /favicon.ico" $ do
      get "/favicon.ico" `shouldRespondWith`
        404 {matchHeaders = ["Content-Type" <:> "text/plain"]}
    it "GET /" $ do
      get "/" `shouldRespondWith`
        200 {matchHeaders = ["Content-Type" <:> "text/html"]}
    it "GET /add" $ do
      get "/add" `shouldRespondWith`
        200 {matchHeaders = ["Content-Type" <:> "text/html"]}
    it "GET /addfile" $ do
      get "/addfile" `shouldRespondWith`
        200 {matchHeaders = ["Content-Type" <:> "text/html"]}
    it "POST /add" $ do
      postHtmlForm "/add" [("secret", "hello")] `shouldRespondWith`
        200 {matchHeaders = ["Content-Type" <:> "text/html"]}
    it "POST /add" $ do
      postHtmlForm "/add" [("foo", "bar")] `shouldRespondWith`
        400 {matchHeaders = ["Content-Type" <:> "text/plain"]}
    it "GET /badness" $ do
      get "/badness" `shouldRespondWith`
        "You are likely to be eaten by a grue" {matchStatus = 404, matchHeaders = ["Content-Type" <:> "text/plain"]}
    -- why doesn't this work? it returns 200. appears not to pass user agent?
    -- it "GET / from Slack" $ do
    --   request methodGet "/" [(hUserAgent, "Slackbot")] "" `shouldRespondWith`
    --     404 {matchHeaders = ["Content-Type" <:> "text/plain"]}
    -- and how to POST a file? in process...
    -- it "POST /addfile" $ do
    --   request methodPost "/addfile" [(hContentType, "multipart/form-data")] "" `shouldRespondWith`
    --     200 {matchHeaders = ["Content-Type" <:> "text/html"]}
