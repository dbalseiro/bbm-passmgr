{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai (Application)

import qualified Web.Scotty as S

import Server (app)

runApplication :: IO Application
runApplication = runAppWith S.scottyApp

runAppWith :: (S.ScottyM () -> IO a) -> IO a
runAppWith f = f app

spec :: Spec
spec = with runApplication $
  describe "GET /" $
    it "responds with 200" $
      get "/healthcheck" `shouldRespondWith` "I'm ok" {matchStatus = 200}

