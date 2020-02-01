module Service (storePassword, validatePassword) where

import Types

import Polysemy (Sem, Members)
import Polysemy.KVStore (KVStore)
import qualified Polysemy.KVStore as KVStore

storePassword
  :: Members '[KVStore Username Password] r
  => Username
  -> Password
  -> Sem r ()
storePassword = KVStore.writeKV

validatePassword
  :: Members '[KVStore Username Password] r
  => Username
  -> Password
  -> Sem r Bool
validatePassword user pass =
  (== Just pass) <$> KVStore.lookupKV user
