{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Crypto.Hash              (Digest)
import qualified Crypto.Hash              as CH
import           Data.Byteable            (toBytes)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Monoid              ((<>))

import           System.IO.Streams        (InputStream, stdin, stdout, write)
import           System.IO.Streams.Crypto (hashInputStream)

import qualified Data.Multihash.Base      as MB
import qualified Data.Multihash.Digest    as MH


-- data Config =
--     Config
--     { file :: Maybe FilePath
--     , algo :: Algorithm
--     , base :: MB.BaseEncoding
--     }

main :: IO ()
main = do
    digest <- pure stdin >>= encodeStream MH.SHA256
    write (encode MB.Base58 MH.SHA256 digest) stdout
  where
    encode base algo = Just . toStrict . (<> "\n") . MB.encode base . MH.encode algo



encodeStream :: MH.HashAlgorithm -> InputStream ByteString -> IO MH.Digest
encodeStream MH.SHA1 inStream = toBytes <$> (hashInputStream inStream :: IO (Digest CH.SHA1))
encodeStream MH.SHA256 inStream = toBytes <$> (hashInputStream inStream :: IO (Digest CH.SHA256))
encodeStream MH.SHA512 inStream = toBytes <$> (hashInputStream inStream :: IO (Digest CH.SHA512))
encodeStream MH.SHA3 inStream = toBytes <$> (hashInputStream inStream :: IO (Digest CH.SHA3_256))
encodeStream MH.BLAKE2B _ = undefined
encodeStream MH.BLAKE2S _ = undefined
