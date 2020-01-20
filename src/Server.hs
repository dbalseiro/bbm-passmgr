{-# LANGUAGE OverloadedStrings #-}

module Server (runApp) where

import qualified Web.Scotty as S

type Port = Int

runApp :: Port -> IO ()
runApp port = runAppWith (S.scotty port)

runAppWith :: (S.ScottyM () -> IO a) -> IO a
runAppWith f = f app

app :: S.ScottyM ()
app =
  S.get  "/healthcheck" $ S.text "I'm ok"
