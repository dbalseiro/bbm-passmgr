module Service (storePassword, validatePassword) where

import Types

import Data.ByteString (ByteString)
import qualified Crypto.Hash.MD5 as MD5

import Database.Redis (Redis)
import qualified Database.Redis as Hedis

storePassword :: Username -> Password -> Redis (Either ByteString ())
storePassword (Username user) pass = do
  let (PasswordHash hash) = hashPassword pass
  mapRedisReply (const ()) <$> Hedis.set user hash

validatePassword :: Username -> Password -> Redis (Either ByteString Bool)
validatePassword (Username user) pass = do
  let (PasswordHash hash) = hashPassword pass
  mapRedisReply (== Just hash) <$> Hedis.get user

mapRedisReply :: (a -> b) -> Either Hedis.Reply a -> Either ByteString b
mapRedisReply _ (Left _) = Left "Internal Server Error"
mapRedisReply f (Right status) = Right $ f status

hashPassword :: Password -> PasswordHash
hashPassword (Password pass) = PasswordHash $ MD5.hash pass
