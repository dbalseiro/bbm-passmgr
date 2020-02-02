module CryptoHash where

import Types
import qualified Polysemy as P

data CryptoHash m a where
  MakeHash :: Password -> CryptoHash m PasswordHash
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

P.makeSem ''CryptoHash
