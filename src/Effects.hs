module Effects
  ( runStorePassword
  , runValidatePassword
  , runCryptoHash
  ) where

import qualified Crypto.Hash.MD5 as MD5

import CryptoHash
import Types
import Service (storePassword, validatePassword)

import qualified Database.Redis as Redis
import Database.Redis (Connection)

import Data.ByteString (ByteString)
import Data.Function ((&))

import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Embed as P
import qualified Polysemy.KVStore as KVStore
import Polysemy (Sem, Members)
import Polysemy.KVStore (KVStore)

runCryptoHash :: (Sem (CryptoHash ': r) a -> Sem r a)
runCryptoHash = P.interpret $ \case
  MakeHash (Password pass) ->
    (pure . PasswordHash . MD5.hash) pass
  ValidateHash (Password pass) (PasswordHash hash) ->
    (pure . (== hash) . MD5.hash) pass

runStorePassword :: Connection -> Username -> Password -> IO (Either ByteString ())
runStorePassword conn user pass = runWithEffects conn $ storePassword user pass

runValidatePassword :: Connection -> Username -> Password -> IO (Either ByteString Bool)
runValidatePassword conn user pass = runWithEffects conn $ validatePassword user pass

runWithEffects
  :: Connection
  -> (forall r. Members '[CryptoHash, KVStore Username PasswordHash] r => Sem r a)
  -> IO (Either ByteString a)
runWithEffects conn pgm = pgm
  & runCryptoHash
  & KVStore.runKVStoreInRedis toByteString
  & P.runEmbedded (Redis.runRedis conn)
  & P.mapError (const "Internal Server Error")
  & P.runError
  & P.runM

