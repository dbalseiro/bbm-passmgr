{-# LANGUAGE OverloadedStrings #-}

module Server (runApp, app) where

import Effects (runStorePassword)

import qualified Web.Scotty as S
import Network.HTTP.Types.Status (status400, statusMessage)

--import qualified Data.Text.Lazy as T
--import qualified Data.Text.Encoding as T

import Database.Redis (Connection)
import qualified Database.Redis as Hedis

import Control.Monad.IO.Class (liftIO)

type Port = Int

runApp :: Port -> IO ()
runApp port = runAppWith (S.scotty port)

runAppWith :: (S.ScottyM () -> IO a) -> IO a
runAppWith f = withDBConnection $ f . app

withDBConnection :: (Connection -> IO a) -> IO a
withDBConnection f =
  Hedis.checkedConnect Hedis.defaultConnectInfo >>= f

app :: Connection -> S.ScottyM ()
app conn = do
  S.get "/healthcheck" $
    S.text "I'm ok"

  S.post "/store" $ do
    user <- S.param "user"
    pass <- S.param "pass"
    liftIO (runStorePassword conn user pass) >>=
      either serverError (const $ S.text "OK")

  where
    serverError err = S.status $ status400 { statusMessage = err }


