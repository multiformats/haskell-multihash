module Main where

import           Crypto.Hash            (Digest, SHA1)
import           Data.Multihash.Streams (hashInputStream)
import           System.IO.Streams      (stdin)


main :: IO ()
main = pure stdin >>= hashInputStream >>= (print :: Digest SHA1 -> IO ())
