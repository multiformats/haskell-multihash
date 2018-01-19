module System.IO.Streams.Crypto where

import Crypto.Hash
  ( Digest
  , HashAlgorithm(..)
  , hashFinalize
  , hashInit
  , hashUpdate
  )
import Data.ByteString (ByteString)
import System.IO.Streams (InputStream, fold)

hashInputStream :: (HashAlgorithm h) => InputStream ByteString -> IO (Digest h)
hashInputStream = fmap hashFinalize . fold hashUpdate hashInit

