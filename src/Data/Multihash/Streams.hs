module Data.Multihash.Streams where

import           Control.Exception     (bracket)
import           Crypto.Hash           (Digest, HashAlgorithm (..))
import           Data.ByteString       (ByteString)
import           Data.Multihash.Digest (decoder)
import           System.IO.Streams     (InputStream, fold, inputFoldM)


hashInputStream :: (HashAlgorithm h) => InputStream ByteString -> IO (Digest h)
hashInputStream = fmap hashFinalize . fold update hashInit
  where update ctx bs = hashUpdates ctx [bs]
