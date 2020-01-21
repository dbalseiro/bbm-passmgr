module Types where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.String (IsString)
import Data.Binary

import Web.Scotty (Parsable(..))

newtype Username = Username ByteString
  deriving (Eq, Show, Ord, IsString, Binary)

newtype Password = Password ByteString
  deriving (Eq, Show, IsString, Binary)

newtype PasswordHash = PasswordHash ByteString
  deriving (Eq, Show, IsString, Binary)

class ToByteString a where
  toByteString :: a -> ByteString

instance ToByteString Username where
  toByteString (Username bs) = bs

instance ToByteString Password where
  toByteString (Password bs) = bs

instance ToByteString PasswordHash where
  toByteString (PasswordHash bs) = bs

instance Parsable Username where
  parseParam = Right . Username . toStrict . encodeUtf8

instance Parsable Password where
  parseParam = Right . Password . toStrict . encodeUtf8
