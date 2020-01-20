module Server (runApp) where

type Port = Int

runApp :: Port -> IO ()
runApp port = putStrLn $ "Server running on port " <> show port
