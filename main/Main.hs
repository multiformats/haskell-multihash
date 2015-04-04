{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Crypto.Hash              (Digest)
import qualified Crypto.Hash              as CH
import           Data.Byteable            (toBytes)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (fromStrict, toStrict)
import           Options.Applicative

import           System.IO.Streams        (InputStream, stdin, stdout,
                                           withFileAsInput, write)
import           System.IO.Streams.Crypto (hashInputStream)

import qualified Data.Multihash.Base      as MB
import qualified Data.Multihash.Digest    as MH


data Termination = Null | Newline deriving (Show, Eq)
data Config =
    Config
    { cfFile :: Maybe FilePath
    , cfAlgo :: MH.HashAlgorithm
    , cfBase :: MB.BaseEncoding
    , cfTerm :: Termination
    } deriving Show


main :: IO ()
main = do
    config <- execParser opts

    digest <- maybe
              (hash (cfAlgo config) stdin)
              (`withFileAsInput` hash (cfAlgo config))
              (cfFile config)

    write (encode config digest) stdout
  where
    encode (Config _file algo base term) =
        Just . toStrict . line term . MB.encode base . MH.encode algo

    line Null    = (<> "\0")
    line Newline = (<> "\n")


hash :: MH.HashAlgorithm -> InputStream ByteString -> IO MH.Digest
hash MH.SHA1 is   = toBytes <$> (hashInputStream is :: IO (Digest CH.SHA1))
hash MH.SHA256 is = toBytes <$> (hashInputStream is :: IO (Digest CH.SHA256))
hash MH.SHA512 is = toBytes <$> (hashInputStream is :: IO (Digest CH.SHA512))
hash MH.SHA3 is   = toBytes <$> (hashInputStream is :: IO (Digest CH.SHA3_256))
hash MH.BLAKE2B _       = undefined
hash MH.BLAKE2S _       = undefined


opts :: ParserInfo Config
opts = info
       (helper <*> (Config <$> fileArg <*> algoOpt <*> baseOpt <*> nullTermFlag))
       (fullDesc
        <> header "Generate a multihash for the given input."
        <> progDesc "Hash from FILE or stdin if not given.")


algoOpt :: Parser MH.HashAlgorithm
algoOpt =
    option auto
    $  long "algorithm"
    <> short 'a'
    <> metavar "ALGO"
    <> showDefault <> value MH.SHA256
    <> help ("Hash algorithm to apply to input " <> show ([minBound..] :: [MH.HashAlgorithm]))


baseOpt :: Parser MB.BaseEncoding
baseOpt =
    option auto
    $  long "encoding"
    <> short 'e'
    <> metavar "ENCODING"
    <> showDefault <> value MB.Base58
    <> help ("Base encoding of output digest " <> show ([minBound..] :: [MB.BaseEncoding]))


nullTermFlag :: Parser Termination
nullTermFlag =
    flag Newline Null
    $  long "print0"
    <> short '0'
    <> help "End filenames with NUL, for use with xargs"


fileArg :: Parser (Maybe FilePath)
fileArg = optional . argument str $ metavar "FILE"
