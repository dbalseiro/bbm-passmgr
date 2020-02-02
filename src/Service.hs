module Service (storePassword, validatePassword) where

import Types
import CryptoHash

import Control.Monad ((<=<))

import Polysemy (Sem, Members)
import Polysemy.KVStore (KVStore)
import qualified Polysemy.KVStore as KVStore

storePassword
  :: Members '[CryptoHash, KVStore Username PasswordHash] r
  => Username
  -> Password
  -> Sem r ()
storePassword user =
  KVStore.writeKV user <=< makeHash

validatePassword
  :: Members '[CryptoHash, KVStore Username PasswordHash] r
  => Username
  -> Password
  -> Sem r Bool
validatePassword user pass =
  KVStore.lookupKV user >>= \case
    Nothing   -> return False
    Just hash -> validateHash pass hash
