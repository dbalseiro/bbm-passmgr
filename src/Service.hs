module Service (storePassword, validatePassword) where

import Types
import CryptoHash

import Polysemy
import Polysemy.KVStore

storePassword
  :: Members '[KVStore Username PasswordHash, CryptoHash] r
  => Username -> Password -> Sem r ()
storePassword user pass = do
  hash <- makeHash pass
  writeKV user hash

validatePassword
  :: Members '[KVStore Username PasswordHash, CryptoHash] r
  => Username -> Password -> Sem r Bool
validatePassword user pass =
  lookupKV user >>= \case
    Nothing -> return False
    Just hash -> validateHash pass hash
