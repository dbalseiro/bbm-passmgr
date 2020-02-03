module Effects (runStorePassword, runValidatePassword) where

import Types
import Service
import CryptoHash

import qualified Crypto.Hash.MD5 as MD5

import Database.Redis

import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.KVStore

import Data.ByteString (ByteString)
import Data.Function ((&))

runStorePassword :: Connection -> Username -> Password -> IO (Either ByteString ())
runStorePassword conn user pass = runEffects conn $ storePassword user pass

runValidatePassword :: Connection -> Username -> Password -> IO (Either ByteString Bool)
runValidatePassword conn user pass = runEffects conn $ validatePassword user pass

runEffects
  :: Connection
  -> (forall r. Members '[KVStore Username PasswordHash, CryptoHash] r => Sem r a)
  -> IO (Either ByteString a)
runEffects conn pgm = pgm
  & runKVStoreInRedis toByteString
  & runEmbedded (runRedis conn)
  & mapError (const "Internal Server Error")
  & runError
  & runCryptoHashPurely (\(Password pass) -> PasswordHash $ MD5.hash pass)
  & runM

runCryptoHashPurely :: (Password -> PasswordHash) -> (Sem (CryptoHash ': r) a -> Sem r a)
runCryptoHashPurely hashfunction =
  interpret $ \case
    MakeHash pass -> return (hashfunction pass)
    ValidateHash pass hash -> return (hash == hashfunction pass)
