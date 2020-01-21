module Effects (runStorePassword) where

import Types
import Service

import qualified Polysemy as P
import qualified Polysemy.KVStore as Store
import qualified Polysemy.Embed as Embed
import qualified Polysemy.Error as Error
import Polysemy.KVStore (KVStore)
import Polysemy (Sem, Members)

import Database.Redis (Connection)
import qualified Database.Redis as Hedis

import Data.Function ((&))
import qualified Data.ByteString.Char8 as B8

runAllEffects
  :: Connection
  -> (forall r. Members '[KVStore Username Password] r => Sem r a)
  -> IO (Either B8.ByteString a)
runAllEffects conn pgm = pgm
  & Store.runKVStoreInRedis toByteString
  & Embed.runEmbedded (Hedis.runRedis conn)
  & Error.mapError toErrMsg
  & Error.runError
  & P.runM

runStorePassword :: Connection -> Username -> Password -> IO (Either B8.ByteString ())
runStorePassword conn username pass =
    runAllEffects conn (storePassword username pass)

toErrMsg :: Hedis.Reply -> B8.ByteString
toErrMsg (Hedis.SingleLine bs)            = bs
toErrMsg (Hedis.Error bs)                 = bs
toErrMsg (Hedis.Bulk (Just bs))           = bs
toErrMsg (Hedis.MultiBulk (Just replies)) = mconcat $ fmap toErrMsg replies
toErrMsg _                                = "Internal Server Error"

