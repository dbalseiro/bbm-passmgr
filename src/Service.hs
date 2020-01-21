module Service (storePassword) where

import Types

import Polysemy (Sem, Members)
import Polysemy.KVStore (KVStore)
import qualified Polysemy.KVStore as Store

storePassword
  :: Members '[KVStore Username Password] r
  => Username
  -> Password
  -> Sem r ()
storePassword = Store.writeKV
