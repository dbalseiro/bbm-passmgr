module Service (storePassword, validatePassword) where

import Types

import Data.Text.Lazy (Text)

storePassword :: Username -> Password -> Either Text ()
storePassword = undefined

validatePassword :: Username -> Password -> Either Text Bool
validatePassword = undefined
