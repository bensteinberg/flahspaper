{-# LANGUAGE OverloadedStrings #-}
module Main where

import Flahspaper
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let sm = Map.empty :: SecretMap
  secrets <- newTVarIO sm

  let sopts = Options (24 * 60 * 60) 10485760

  forkIO $ janitor secrets sopts

  let tlsConfig = tlsSettings "certificate.pem" "key.pem"
      config = setPort 8443 defaultSettings

  runTLS tlsConfig config $ app secrets sopts
