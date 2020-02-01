module Effects (runStorePassword, runValidatePassword) where

import Types
import Service (storePassword, validatePassword)

import qualified Database.Redis as Redis
import Database.Redis (Connection)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as T
import Data.Text.Lazy (Text)
import Data.ByteString (ByteString)
import Data.Function ((&))

import qualified Polysemy.Error as P
import qualified Polysemy.Embed as P
import qualified Polysemy as P
import qualified Polysemy.KVStore as KVStore
import Polysemy (Sem, Members)
import Polysemy.KVStore (KVStore)

runStorePassword :: Connection -> Username -> Password -> IO (Either Text ())
runStorePassword conn user pass = runWithEffects conn $ storePassword user pass

runValidatePassword :: Connection -> Username -> Password -> IO (Either Text Bool)
runValidatePassword conn user pass = runWithEffects conn $ validatePassword user pass

runWithEffects
  :: Connection
  -> (forall r. Members '[KVStore Username Password] r => Sem r a)
  -> IO (Either Text a)
runWithEffects conn pgm = pgm
  & KVStore.runKVStoreInRedis toByteString
  & P.runEmbedded (Redis.runRedis conn)
  & P.mapError toErrMsg
  & P.runError
  & P.runM

toErrMsg :: Redis.Reply -> Text
toErrMsg (Redis.SingleLine bs) = toText bs
toErrMsg (Redis.Error bs) = toText bs
toErrMsg (Redis.Bulk (Just bs)) = toText bs
toErrMsg (Redis.MultiBulk (Just replies)) = mconcat $ fmap toErrMsg replies
toErrMsg _ = "Internal Server Error"

toText :: ByteString -> Text
toText = T.fromStrict . T.decodeUtf8