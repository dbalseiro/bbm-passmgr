module Server (runApp) where

import Effects (runStorePassword, runValidatePassword)

import qualified Web.Scotty as S

import Network.HTTP.Types.Status (status500, status400, statusMessage)

import Control.Monad.IO.Class (liftIO)

import Database.Redis (Connection)
import qualified Database.Redis as Hedis

type Port = Int

runApp :: Port -> IO ()
runApp port = runAppWith (S.scotty port)

runAppWith :: (S.ScottyM () -> IO a) -> IO a
runAppWith f = withRedis $ f . api

withRedis :: (Connection -> IO a) -> IO a
withRedis f = Hedis.checkedConnect Hedis.defaultConnectInfo >>= f

api :: Connection -> S.ScottyM ()
api conn = do
  S.post "/store" $ do
    user <- S.param "user"
    pass <- S.param "pass"
    liftIO (runStorePassword conn user pass) >>= \case
      Left err -> serverError err
      Right () -> S.text "Password stored"

  S.post "/validate" $ do
    user <- S.param "user"
    pass <- S.param "pass"
    liftIO (runValidatePassword conn user pass) >>= \case
      Left err    -> serverError err
      Right False -> invalidPassword
      Right True  -> S.text "OK"

  where
    serverError err = S.status $ status500 { statusMessage = err }
    invalidPassword = S.status $ status400 { statusMessage = "Invalid Username/Password" }

