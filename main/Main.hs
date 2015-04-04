{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Crypto.Hash              (Digest)
import qualified Crypto.Hash              as CH
import           Data.Byteable            (toBytes)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (toStrict)
import           Options.Applicative

import           System.IO.Streams        (InputStream, stdin, stdout, write)
import           System.IO.Streams.Crypto (hashInputStream)

import qualified Data.Multihash.Base      as MB
import qualified Data.Multihash.Digest    as MH


data Config = Config MH.HashAlgorithm MB.BaseEncoding Termination deriving Show
data Termination = Null | Newline deriving (Show, Eq)


main :: IO ()
main = do
    (Config algo base term) <- execParser opts
    digest <- encodeStream algo stdin
    write (encode base algo term digest) stdout
  where
    encode base algo term = Just . toStrict . line term . MB.encode base . MH.encode algo

    line Null    = (<> "\0")
    line Newline = (<> "\n")

encodeStream :: MH.HashAlgorithm -> InputStream ByteString -> IO MH.Digest
encodeStream MH.SHA1 inStream = toBytes <$> (hashInputStream inStream :: IO (Digest CH.SHA1))
encodeStream MH.SHA256 inStream = toBytes <$> (hashInputStream inStream :: IO (Digest CH.SHA256))
encodeStream MH.SHA512 inStream = toBytes <$> (hashInputStream inStream :: IO (Digest CH.SHA512))
encodeStream MH.SHA3 inStream = toBytes <$> (hashInputStream inStream :: IO (Digest CH.SHA3_256))
encodeStream MH.BLAKE2B _ = undefined
encodeStream MH.BLAKE2S _ = undefined


opts :: ParserInfo Config
opts = info (helper <*> (Config <$> algoOpt <*> baseOpt <*> nullTermFlag))
       (fullDesc
        <> header "Generate a multihash for the given input.")


algoOpt :: Parser MH.HashAlgorithm
algoOpt =
    option auto
    $  long "algorithm"
    <> short 'a'
    <> metavar "ALGO"
    <> help "Hash algorithm to apply to input"
    <> showDefault <> value MH.SHA256


baseOpt :: Parser MB.BaseEncoding
baseOpt =
    option auto
    $  long "encoding"
    <> short 'e'
    <> metavar "ENCODING"
    <> help "Base encoding of output digest"
    <> showDefault <> value MB.Base58


nullTermFlag :: Parser Termination
nullTermFlag =
    flag Newline Null
    $  long "print0"
    <> short '0'
    <> help "End filenames with NUL, for use with xargs"
