module CryptoHash where

import qualified Polysemy as P
import Types

data CryptoHash m a where
  MakeHash :: Password -> CryptoHash m PasswordHash
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

P.makeSem ''CryptoHash
