{-# LANGUAGE OverloadedStrings #-}

module Server (runApp, app) where

import Service (storePassword, validatePassword)

import qualified Web.Scotty as S

import Network.HTTP.Types.Status (status500, status400, statusMessage)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as T


type Port = Int

runApp :: Port -> IO ()
runApp port = runAppWith (S.scotty port)

runAppWith :: (S.ScottyM () -> IO a) -> IO a
runAppWith f = f app

app :: S.ScottyM ()
app = do
  S.get "/healthcheck" $
    S.text "I'm ok"

  S.post "/store" $ do
    user <- S.param "user"
    pass <- S.param "pass"
    either serverError (const $ S.text "OK") (storePassword user pass)

  S.post "/validate" $ do
    user <- S.param "user"
    pass <- S.param "pass"
    either serverError respond (validatePassword user pass)

  where
    serverError err = S.status $ status500 { statusMessage = T.encodeUtf8 (T.toStrict err) }

    respond False = S.status $ status400 { statusMessage = "Invalid Username/Password" }
    respond True  = S.text "OK"
