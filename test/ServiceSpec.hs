module ServiceSpec (spec) where

import Test.QuickCheck.Instances ()
import Test.QuickCheck
import Test.Hspec

import Types
import Service
import CryptoHash
import Effects (runCryptoHash)

import Data.Function ((&))
import qualified Data.Map as Map

import qualified Polysemy as P
import qualified Polysemy.KVStore as KVStore
import Polysemy (Sem, Members)
import Polysemy.KVStore (KVStore)

spec :: Spec
spec =
  describe "password manager" $
    it "works on random input" $
      property $ \user pass -> addAndValidate (Username user) (Password pass)

addAndValidate :: Username -> Password -> Bool
addAndValidate user pass = runWithEffects $ do
  storePassword user pass
  validatePassword user pass

runWithEffects
  :: (forall r. Members '[CryptoHash, KVStore Username PasswordHash] r => Sem r a)
  -> a
runWithEffects pgm = pgm
  & runCryptoHash
  & KVStore.runKVStorePurely Map.empty
  & P.run
  & snd
