module Service (storePassword, validatePassword) where

import Types

import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as T

import Database.Redis (Redis)
import qualified Database.Redis as Hedis

storePassword :: Username -> Password -> Redis (Either Text ())
storePassword (Username user) (Password pass) =
  mapRedisReply (const ()) <$> Hedis.set user pass

validatePassword :: Username -> Password -> Redis (Either Text Bool)
validatePassword (Username user) (Password pass) =
  mapRedisReply (== Just pass) <$> Hedis.get user

mapRedisReply :: (a -> b) -> Either Hedis.Reply a -> Either Text b
mapRedisReply _ (Left reply)   = (Left . T.fromStrict . T.decodeUtf8 . toErrMsg) reply
mapRedisReply f (Right status) = Right $ f status

toErrMsg :: Hedis.Reply -> ByteString
toErrMsg (Hedis.SingleLine bs) = bs
toErrMsg (Hedis.Error bs) = bs
toErrMsg (Hedis.Bulk (Just bs)) = bs
toErrMsg (Hedis.MultiBulk (Just replies)) = mconcat $ fmap toErrMsg replies
toErrMsg _ = "Internal Server Error"
